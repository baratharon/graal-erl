/*
 * Copyright (c) 2012, 2014, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * The Universal Permissive License (UPL), Version 1.0
 *
 * Subject to the condition set forth below, permission is hereby granted to any
 * person obtaining a copy of this software, associated documentation and/or
 * data (collectively the "Software"), free of charge and under any and all
 * copyright rights in the Software, and any and all patent rights owned or
 * freely licensable by each licensor hereunder covering either (i) the
 * unmodified Software as contributed to or provided by such licensor, or (ii)
 * the Larger Works (as defined below), to deal in both
 *
 * (a) the Software, and
 *
 * (b) any piece of software and/or hardware listed in the lrgrwrks.txt file if
 * one is included with the Software each a "Larger Work" to which the Software
 * is contributed by such licensors),
 *
 * without restriction, including without limitation the rights to copy, create
 * derivative works of, display, perform, and distribute the Software and make,
 * use, sell, offer for sale, import, export, have made, and have sold the
 * Software and the Larger Work(s), and to sublicense the foregoing rights on
 * either these or other terms.
 *
 * This license is subject to the following condition:
 *
 * The above copyright notice and either this complete permission notice or at a
 * minimum a reference to the UPL must be included in all copies or substantial
 * portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package com.oracle.truffle.erl.runtime;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.Charset;
import java.util.Comparator;
import java.util.HashSet;
import java.util.concurrent.ExecutionException;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import com.oracle.truffle.api.ExecutionContext;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.erl.ErlangLanguage;
import com.oracle.truffle.erl.MFA;
import com.oracle.truffle.erl.builtins.ErlBuiltinNode;
import com.oracle.truffle.erl.nodes.ErlExpressionNode;
import com.oracle.truffle.erl.nodes.ErlRootNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.nodes.local.ErlReadArgumentNode;
import com.oracle.truffle.erl.parser.ErlParser;
import com.oracle.truffle.erl.runtime.builtins.ETSBuiltins;
import com.oracle.truffle.erl.runtime.builtins.ErlangBuiltins;
import com.oracle.truffle.erl.runtime.builtins.FileBuiltins;
import com.oracle.truffle.erl.runtime.builtins.IOBuiltins;
import com.oracle.truffle.erl.runtime.builtins.ListsBuiltins;
import com.oracle.truffle.erl.runtime.builtins.MapsBuiltins;
import com.oracle.truffle.erl.runtime.builtins.MathBuiltins;
import com.oracle.truffle.erl.runtime.builtins.NetKernelBuiltins;
import com.oracle.truffle.erl.runtime.builtins.OSBuiltins;
import com.oracle.truffle.erl.runtime.builtins.PrimEvalBuiltins;
import com.oracle.truffle.erl.runtime.builtins.PrimFileBuiltins;
import com.oracle.truffle.erl.runtime.builtins.ReBuiltins;
import com.oracle.truffle.erl.runtime.builtins.TruffleBuiltins;
import com.oracle.truffle.erl.runtime.builtins.UnicodeBuiltins;

/**
 * The run-time state of Erlang during execution. One context is instantiated before any source code
 * is parsed, and this context is passed around to all methods that need access to it.
 * <p>
 * It would be an error to have two different context instances during the execution of one script.
 * However, if two separate scripts run in one Java VM at the same time, they have a different
 * context. Therefore, the context is not a singleton.
 */
public final class ErlContext extends ExecutionContext {

    private final ErlangLanguage language;
    private final BufferedReader input;
    private final PrintWriter output;
    private final ErlModuleRegistry moduleRegistry;
    private final ErlProcess.ProcessManager processManager = new ErlProcess.ProcessManager();

    public ErlContext(ErlangLanguage language, BufferedReader input, PrintWriter output) {
        this(language, input, output, true);
    }

    public ErlContext(ErlangLanguage language) {
        this(language, null, null, false);
    }

    private ErlContext(ErlangLanguage language, BufferedReader input, PrintWriter output, boolean installBuiltins) {
        this.language = language;
        this.input = input;
        this.output = output;
        this.moduleRegistry = new ErlModuleRegistry(this);
        installBuiltins(installBuiltins);
    }

    public static final boolean IS_LITTLE_ENDIAN = ByteOrder.nativeOrder().equals(ByteOrder.LITTLE_ENDIAN);
    public static final String OS_NAME = System.getProperty("os.name");
    public static final String OS_NAME_LOWER = System.getProperty("os.name").toLowerCase();
    public static final String OS_ARCH = System.getProperty("os.arch");

    public void close() {
        processManager.close();
    }

    public ErlProcess.ProcessManager getProcessManager() {
        return processManager;
    }

    public BufferedReader getInput() {
        return input;
    }

    public PrintWriter getOutput() {
        return output;
    }

    /**
     * Returns the registry of all modules that are currently loaded.
     */
    public ErlModuleRegistry getModuleRegistry() {
        return moduleRegistry;
    }

    public ErlangLanguage getLanguage() {
        return language;
    }

    /**
     * Adds all built-in functions to the {@link ErlModuleRegistry}. This method lists all
     * {@link ErlBuiltinNode built-in implementation classes}.
     */
    private void installBuiltins(final boolean registerRootNodes) {

        // little trick to avoid duplicate names in different modules,
        // like erlang:put and maps:put

        ErlangBuiltins.install(this, registerRootNodes);
        FileBuiltins.install(this, registerRootNodes);
        PrimFileBuiltins.install(this, registerRootNodes);
        ListsBuiltins.install(this, registerRootNodes);
        MapsBuiltins.install(this, registerRootNodes);
        NetKernelBuiltins.install(this, registerRootNodes);
        TruffleBuiltins.install(this, registerRootNodes);
        UnicodeBuiltins.install(this, registerRootNodes);
        ETSBuiltins.install(this, registerRootNodes);
        OSBuiltins.install(this, registerRootNodes);
        IOBuiltins.install(this, registerRootNodes);
        ReBuiltins.install(this, registerRootNodes);
        MathBuiltins.install(this, registerRootNodes);
        PrimEvalBuiltins.install(this, registerRootNodes);
    }

    public static ErlBuiltinNode makeBuiltin(NodeFactory<? extends ErlBuiltinNode> factory) {
        return makeBuiltin(null, factory);
    }

    public static ErlBuiltinNode makeBuiltin(final Object optionalArgument, NodeFactory<? extends ErlBuiltinNode> factory) {
        /*
         * The builtin node factory is a class that is automatically generated by the Truffle DSL.
         * The signature returned by the factory reflects the signature of the @Specialization
         * methods in the builtin classes.
         */
        int argumentCount = factory.getExecutionSignature().size();
        ErlExpressionNode[] argumentNodes = new ErlExpressionNode[argumentCount];
        /*
         * Builtin functions are like normal functions, i.e., the arguments are passed in as an
         * Object[] array encapsulated in ErlArguments. A ErlReadArgumentNode extracts a parameter
         * from this array.
         */
        for (int i = 0; i < argumentCount; i++) {
            argumentNodes[i] = new ErlReadArgumentNode(null, i);
        }
        /* Instantiate the builtin node. This node performs the actual functionality. */
        if (null == optionalArgument) {
            return factory.createNode((Object) argumentNodes);
        } else {
            return factory.createNode(optionalArgument, argumentNodes);
        }
    }

    public static ErlRootNode wrapBuiltinBodyNode(final ErlExpressionNode nodeToWrap, final MFA mfa) {
        return new ErlRootNode(new FrameDescriptor(), nodeToWrap, mfa);
    }

    public void installBuiltin(NodeFactory<? extends ErlBuiltinNode> factory, boolean registerRootNodes) {

        int argumentCount = factory.getExecutionSignature().size();
        ErlBuiltinNode builtinBodyNode = makeBuiltin(factory);

        final MFA mfa = builtinBodyNode.getName();

        final String moduleName = mfa.getModule();
        final String funcName = mfa.getFunction();
        final int arity = mfa.getArity();

        assert arity == argumentCount;

        /* Wrap the builtin in a RootNode. Truffle requires all AST to start with a RootNode. */
        ErlRootNode rootNode = wrapBuiltinBodyNode(builtinBodyNode, mfa);

        if (registerRootNodes) {
            /* Register the builtin function in our function registry. */
            moduleRegistry.registerBIF(moduleName, funcName, arity, rootNode);
        }
    }

    public void installWrappedExpressionBuiltin(final MFA mfa, ErlExpressionNode exprBodyNode, boolean registerRootNodes) {

        final String moduleName = mfa.getModule();
        final String funcName = mfa.getFunction();
        final int arity = mfa.getArity();

        /* Wrap the builtin in a RootNode. Truffle requires all AST to start with a RootNode. */
        ErlRootNode rootNode = wrapBuiltinBodyNode(exprBodyNode, mfa);

        if (registerRootNodes) {
            /* Register the builtin function in our function registry. */
            moduleRegistry.registerBIF(moduleName, funcName, arity, rootNode);
        }
    }

    public static NodeInfo lookupNodeInfo(Class<?> clazz) {
        if (clazz == null) {
            return null;
        }
        NodeInfo info = clazz.getAnnotation(NodeInfo.class);
        if (info != null) {
            return info;
        } else {
            return lookupNodeInfo(clazz.getSuperclass());
        }
    }

    /**
     * Evaluate a source, causing any definitions to be registered (but not executed).
     *
     * @param source The {@link Source} to parse.
     */
    public ErlModuleImpl evalSource(Source source) {

        final ErlModuleImpl module = ErlParser.parseErlang(source);

        if (null != module) {
            moduleRegistry.register(module);
        }

        return module;
    }

    /**
     * Evaluate a preprocessed source, causing any definitions to be registered (but not executed).
     * The preprocessed source is a easy-to-load Erlang AST file.
     *
     * @param source The {@link Source} to parse.
     */
    public ErlModuleImpl evalPreprocessed(Source source, final boolean preLoaded) {

        final ErlModuleImpl module = ErlParser.parseErlangPreprocessed(preLoaded, null, source.getReader());

        if (null != module) {
            moduleRegistry.register(module);
        }

        return module;
    }

    /**
     * <b>For internal purposes only.</b>
     * <p>
     * Loads a module from its <code>.erl</code> file.
     *
     * @param moduleName name of the module to load
     * @return <code>true</code> when successfully loaded, <code>false</code> otherwise
     */
    public boolean loadModule(String moduleName) {

        if (null == codeGetPath) {
            codeGetPath = moduleRegistry.functionLookup(MFA_CODE_GETPATH);
        }

        if (null != codeGetPath) {
            try {
                final Object result = ErlProcess.spawn(this, codeGetPath, new Object[0]).getFuture().get();

                if (null != result && result instanceof ErlList) {
                    for (Object pathObj : ((ErlList) result).toArray()) {
                        if (pathObj instanceof ErlList) {
                            String path = ((ErlList) pathObj).toString();
                            if (path.length() > 2) {
                                path = path.substring(1, path.length() - 1);

                                if (path.endsWith("/beam") || path.endsWith("\\beam")) {
                                    path = path.substring(0, path.length() - 4) + "src";
                                }

                                final String basename = path + "/" + moduleName;

                                File file;

                                file = new File(basename + ErlangLanguage.ERL_AST_EXTENSION);
                                if (file.exists() && file.isFile()) {
                                    evalPreprocessed(Source.fromFileName(file.getAbsolutePath()).withMimeType(ErlangLanguage.ERL_MIME_TYPE), false);
                                    return true;
                                }

                                file = new File(basename + ErlangLanguage.ERL_SOURCE_EXTENSION);
                                if (file.exists() && file.isFile()) {
                                    evalSource(Source.fromFileName(file.getAbsolutePath()).withMimeType(ErlangLanguage.ERL_MIME_TYPE));
                                    return true;
                                }
                            }
                        }
                    }
                }

            } catch (InterruptedException | ExecutionException | IOException e) {
                return false;
            }
        }

        return false;
    }

    private static final MFA MFA_CODE_GETPATH = new MFA("code", "get_path", 0);
    private ErlFunction codeGetPath = null;

    public static Object fromForeignValue(Object a) {
        if (a instanceof Long || a instanceof BigInteger) {
            return a;
        } else if (a instanceof Number) {
            return ((Number) a).longValue();
        }
        return a;
    }

    public void waitForTerminateAll() {
        processManager.waitForTerminateAll();
    }

    public static boolean isWindows() {
        return (OS_NAME_LOWER.indexOf("win") >= 0);
    }

    public static boolean isUnix() {
        return (OS_NAME_LOWER.indexOf("nix") >= 0 || OS_NAME_LOWER.indexOf("nux") >= 0 || OS_NAME_LOWER.indexOf("aix") > 0);
    }

    public static boolean isMac() {
        return (OS_NAME_LOWER.indexOf("mac") >= 0);
    }

    private static final HashSet<String> keywords = new HashSet<>();

    static {
        keywords.add("after");
        keywords.add("begin");
        keywords.add("case");
        keywords.add("catch");
        keywords.add("cond");
        keywords.add("end");
        keywords.add("fun");
        keywords.add("if");
        keywords.add("let");
        keywords.add("of");
        keywords.add("query");
        keywords.add("receive");
        keywords.add("when");
        keywords.add("and");
        keywords.add("band");
        keywords.add("bnot");
        keywords.add("bor");
        keywords.add("bsl");
        keywords.add("bsr");
        keywords.add("bxor");
        keywords.add("div");
        keywords.add("not");
        keywords.add("or");
        keywords.add("rem");
        keywords.add("xor");
    }

    public static boolean isKeyword(final String str) {
        return keywords.contains(str);
    }

    public static boolean decodeBoolean(ErlAtom value) {

        // fast comparisons first

        if (ErlAtom.TRUE == value) {
            return true;
        }

        if (ErlAtom.FALSE == value) {
            return false;
        }

        // then slow comparisons

        if (ErlAtom.TRUE.getValue().equals(value.getValue())) {
            return true;
        }

        if (ErlAtom.FALSE.getValue().equals(value.getValue())) {
            return false;
        }

        throw ErlControlException.makeBadarg();
    }

    public static double decodeDouble(Object obj) {

        if (obj instanceof Double) {

            return (double) obj;
        }

        throw ErlControlException.makeBadarg();
    }

    public static double toDouble(Object obj) {

        if (obj instanceof Double) {

            return (double) obj;

        } else if (obj instanceof Long) {

            return (long) obj;

        } else if (obj instanceof BigInteger) {

            return ((BigInteger) obj).doubleValue();
        }

        throw ErlControlException.makeBadarg();
    }

    public static long decodeLong(Object obj) {

        if (obj instanceof Long) {

            return (long) obj;

        } else if (obj instanceof BigInteger) {

            try {
                return ((BigInteger) obj).longValueExact();
            } catch (ArithmeticException ex) {
                // throw the exception...
            }
        }

        throw ErlControlException.makeBadarg();
    }

    public static int decodeInt(Object obj) {

        if (obj instanceof Long) {
            if (((long) obj) >= Integer.MIN_VALUE && ((long) obj) <= Integer.MAX_VALUE) {
                return (int) ((long) obj);
            }
        } else if (obj instanceof BigInteger) {
            try {
                return ((BigInteger) obj).intValueExact();
            } catch (ArithmeticException ex) {
                // throw the exception...
            }
        }

        throw ErlControlException.makeBadarg();
    }

    public enum NumberKind {
        LONG,
        BIGINTEGER,
        DOUBLE
    }

    public static NumberKind getNumberKind(Object number) {

        if (number instanceof Long) {
            return NumberKind.LONG;
        }

        if (number instanceof BigInteger) {
            return NumberKind.BIGINTEGER;
        }

        if (number instanceof Double) {
            return NumberKind.DOUBLE;
        }

        throw new RuntimeException("Cannot determine number kind for: \"" + number + "\"");
    }

    public enum TermRank {
        NUMBER,
        ATOM,
        REFERENCE,
        FUN,
        PORT,
        PID,
        TUPLE,
        MAP,
        NIL,
        LIST,
        BIT_STRING
    }

    /**
     * Determine rank of a term according to the following order:
     *
     * <code>number < atom < reference < fun < port < pid < tuple < map < nil < list < bit string</code>
     *
     * The rank is used when comparing different data types.
     *
     * @param obj
     * @return term rank which is able to comparison
     */
    public static TermRank getTermRank(Object obj) {

        if (obj instanceof Long || obj instanceof BigInteger || obj instanceof Double) {
            return TermRank.NUMBER;
        }

        // Note that, boolean values are atom too.
        if (obj instanceof ErlAtom || obj instanceof Boolean) {
            return TermRank.ATOM;
        }

        if (obj instanceof ErlRef) {
            return TermRank.REFERENCE;
        }

        if (obj instanceof ErlFunction) {
            return TermRank.FUN;
        }

        if (obj instanceof ErlPort) {
            return TermRank.PORT;
        }

        if (obj instanceof ErlPid) {
            return TermRank.PID;
        }

        if (obj instanceof ErlTuple) {
            return TermRank.TUPLE;
        }

        if (obj instanceof ErlMap) {
            return TermRank.MAP;
        }

        if (ErlList.NIL == obj) {
            return TermRank.NIL;
        }

        if (obj instanceof ErlList) {
            return TermRank.LIST;
        }

        if (obj instanceof ErlBinary || obj instanceof ErlLazyBinary || obj instanceof ErlBinaryView) {
            return TermRank.BIT_STRING;
        }

        throw new RuntimeException("Cannot determine term rank for: \"" + obj + "\"");
    }

    @TruffleBoundary // TODO
    public static int compareTerms(Object lhs, Object rhs, boolean exact) {

        final TermRank lhsRank = getTermRank(lhs);
        final TermRank rhsRank = getTermRank(rhs);

        if (lhsRank.ordinal() < rhsRank.ordinal()) {
            return -1;
        } else if (lhsRank.ordinal() > rhsRank.ordinal()) {
            return 1;
        }

        assert lhsRank == rhsRank;

        switch (lhsRank) {
            case NUMBER:
                return compareNumbers(lhs, rhs, exact);

            case ATOM:
                return ErlAtom.compare(lhs, rhs);

            case REFERENCE:
                return ((ErlRef) lhs).compare((ErlRef) rhs);

            case FUN:
                return ((ErlFunction) lhs).compare((ErlFunction) rhs);

            case PORT:
                return ((ErlPort) lhs).compare((ErlPort) rhs);

            case PID:
                return ((ErlPid) lhs).compare((ErlPid) rhs);

            case TUPLE:
                return ((ErlTuple) lhs).compare((ErlTuple) rhs, exact);

            case MAP:
                return ((ErlMap) lhs).compare((ErlMap) rhs, exact);

            case NIL:
                return 0; // nil == nil

            case LIST:
                return ((ErlList) lhs).compare((ErlList) rhs, exact);

            case BIT_STRING: {
                return ErlBinary.fromObject(lhs).compare(ErlBinary.fromObject(rhs));
            }
        }

        throw new RuntimeException("Cannot compare " + lhsRank);
    }

    @TruffleBoundary // TODO
    public static int compareNumbers(Object lhs, Object rhs, boolean exact) {

        final NumberKind lhsKind = getNumberKind(lhs);
        final NumberKind rhsKind = getNumberKind(rhs);

        if (exact) {
            if (lhsKind != NumberKind.DOUBLE && rhsKind == NumberKind.DOUBLE) {
                return -1;
            }

            if (lhsKind == NumberKind.DOUBLE && rhsKind != NumberKind.DOUBLE) {
                return 1;
            }
        }

        switch (lhsKind) {
            case LONG:
                switch (rhsKind) {
                    case LONG:
                        return Long.compare((long) lhs, (long) rhs);

                    case BIGINTEGER:
                        return BigInteger.valueOf((long) lhs).compareTo((BigInteger) rhs);

                    case DOUBLE:
                        return Double.compare((long) lhs, (double) rhs);
                }
                break;

            case BIGINTEGER:
                switch (rhsKind) {
                    case LONG:
                        return ((BigInteger) lhs).compareTo(BigInteger.valueOf((long) rhs));

                    case BIGINTEGER:
                        return ((BigInteger) lhs).compareTo((BigInteger) rhs);

                    case DOUBLE:
                        return Double.compare(((BigInteger) lhs).doubleValue(), (double) rhs);
                }
                break;

            case DOUBLE:
                switch (rhsKind) {
                    case LONG:
                        return Double.compare((double) lhs, (long) rhs);

                    case BIGINTEGER:
                        return Double.compare((double) lhs, ((BigInteger) rhs).doubleValue());

                    case DOUBLE:
                        return Double.compare((double) lhs, (double) rhs);
                }
                break;
        }

        throw new RuntimeException("Cannot compare " + lhsKind + " with " + rhsKind);
    }

    public static boolean isByte(Object obj) {
        return isByte(obj, null);
    }

    public static boolean isByte(Object obj, byte[] refByte) {

        if (obj instanceof Long) {
            return isByte((long) obj, refByte);
        }

        if (obj instanceof BigInteger) {
            BigInteger bi = (BigInteger) obj;

            if (bi.signum() < 0) {
                return false;
            }

            try {
                return isByte(bi.longValueExact(), refByte);
            } catch (ArithmeticException ex) {
                return false;
            }
        }

        return false;
    }

    private static boolean isByte(long l, byte[] refByte) {

        if (180 == l) {
            refByte[0] = 0;
        }

        final byte b = (byte) (l & 0xff);

        if (l == Byte.toUnsignedLong(b)) {
            if (null != refByte) {
                refByte[0] = b;
            }

            return true;
        }

        return false;
    }

    public static boolean isPrintableCharacter(Object obj) {
        return isPrintableCharacter(obj, null);
    }

    public static boolean isPrintableCharacter(Object obj, char[] refChar) {
        byte[] refByte = new byte[1];
        return isByte(obj, refByte) && isPrintableCharacter(refByte[0], refChar);
    }

    public static boolean isPrintableCharacter(byte b, char[] refChar) {
        return isPrintableCharacter(b, refChar, true);
    }

    public static boolean isPrintableCharacter(byte b, char[] refChar, boolean allowExtended) {

        final int code = Byte.toUnsignedInt(b);

        if ((8 <= code && code <= 13) || 27 == code || (32 <= code && code <= 126) || (allowExtended && 160 <= code && code <= 255)) {

            if (null != refChar) {
                refChar[0] = (char) b;
            }

            return true;
        }

        return false;
    }

    public static final Charset LATIN1_CHARSET = Charset.forName("ISO-8859-1");
    public static final Charset UTF8_CHARSET = Charset.forName("UTF-8");

    public static String stringifyPrintableCharacter(final char ch) {
        switch (ch) {
            case 8:
                return "\\b";
            case 9:
                return "\\t";
            case 10:
                return "\\n";
            case 11:
                return "\\v";
            case 12:
                return "\\f";
            case 13:
                return "\\r";
            case 27:
                return "\\e";
            case '\"':
                return "\\\"";
            case '\\':
                return "\\\\";
            default:

                if (ch > 128) {
                    return LATIN1_CHARSET.decode(ByteBuffer.wrap(new byte[]{(byte) ch})).toString();
                }

                return "" + ch;
        }
    }

    public static class TermComparator implements Comparator<Object> {

        private final boolean exact;

        private TermComparator(boolean exact) {
            this.exact = exact;
        }

        public int compare(Object lhs, Object rhs) {
            return ErlContext.compareTerms(lhs, rhs, exact);
        }
    }

    public static final TermComparator TERM_COMPARATOR_EXACT = new TermComparator(true);
    public static final TermComparator TERM_COMPARATOR_EQUAL = new TermComparator(false);

    public static class ListBuilderConsumer implements Consumer<Object> {

        private ErlList result = ErlList.NIL;

        public void accept(Object arg) {
            result = new ErlList(arg, result);
        }

        public ErlList getResult() {
            return result;
        }
    }

    public static class PairListBuilderBiConsumer implements BiConsumer<Object, Object> {

        private ErlList result = ErlList.NIL;

        public void accept(Object arg1, Object arg2) {
            ErlTuple tuple = new ErlTuple(arg1, arg2);
            result = new ErlList(tuple, result);
        }

        public ErlList getResult() {
            return result;
        }
    }

    public static class FirstElementListBuilderBiConsumer implements BiConsumer<Object, Object> {

        private ErlList result = ErlList.NIL;

        public void accept(Object arg1, Object arg2) {
            result = new ErlList(arg1, result);
        }

        public ErlList getResult() {
            return result;
        }
    }

    public static class SecondElementListBuilderBiConsumer implements BiConsumer<Object, Object> {

        private ErlList result = ErlList.NIL;

        public void accept(Object arg1, Object arg2) {
            result = new ErlList(arg2, result);
        }

        public ErlList getResult() {
            return result;
        }
    }

    public static class IfFirstElementListBuilderBiConsumer implements BiConsumer<Object, Object> {

        private ErlList result = ErlList.NIL;
        private final Object ref;

        public IfFirstElementListBuilderBiConsumer(Object ref) {
            this.ref = ref;
        }

        public void accept(Object arg1, Object arg2) {
            if (0 == ErlContext.compareTerms(ref, arg2, true)) {
                result = new ErlList(arg1, result);
            }
        }

        public ErlList getResult() {
            return result;
        }
    }

    public static void notImplemented() {

        System.err.println("**** NOT IMPLEMENTED ****");

        try {
            throw new RuntimeException();
        } catch (RuntimeException ex) {
            ex.printStackTrace();
        }

        System.err.println("**** NOT IMPLEMENTED ****");
        System.err.flush();

        throw ErlControlException.makeBadarg();
    }
}
