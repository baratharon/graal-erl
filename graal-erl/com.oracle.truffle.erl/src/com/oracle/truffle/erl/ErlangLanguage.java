/*
 * Copyright (c) 2012, 2015, Oracle and/or its affiliates. All rights reserved.
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
package com.oracle.truffle.erl;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.UnsupportedSpecializationException;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.instrument.Visualizer;
import com.oracle.truffle.api.instrument.WrapperNode;
import com.oracle.truffle.api.nodes.GraphPrintVisitor;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.NodeUtil;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.api.vm.PolyglotEngine;
import com.oracle.truffle.erl.builtins.ErlBuiltinNode;
import com.oracle.truffle.erl.nodes.ErlExpressionNode;
import com.oracle.truffle.erl.nodes.call.ErlUndefinedFunctionException;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.nodes.instrument.ErlDefaultVisualizer;
import com.oracle.truffle.erl.nodes.instrument.ErlExpressionWrapperNode;
import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlBinary;
import com.oracle.truffle.erl.runtime.ErlBinaryView;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlFunction;
import com.oracle.truffle.erl.runtime.ErlLazyBinary;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.ErlModuleImpl;
import com.oracle.truffle.erl.runtime.ErlModuleRegistry;
import com.oracle.truffle.erl.runtime.ErlPid;
import com.oracle.truffle.erl.runtime.ErlPort;
import com.oracle.truffle.erl.runtime.ErlRef;
import com.oracle.truffle.erl.runtime.ErlTuple;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.stream.Stream;

/**
 * TODO
 */
@TruffleLanguage.Registration(name = "Erlang", version = "0.1", mimeType = ErlangLanguage.ERL_MIME_TYPE)
public final class ErlangLanguage extends TruffleLanguage<ErlContext> {
    private static List<NodeFactory<? extends ErlBuiltinNode>> builtins = Collections.emptyList();
    private static Visualizer visualizer = new ErlDefaultVisualizer();

    private ErlangLanguage() {
    }

    public static final ErlangLanguage INSTANCE = new ErlangLanguage();
    public static final String ERL_MIME_TYPE = "text/x-erlang";

    private static final String OTP_RING0_MODULE = "otp_ring0";
    private static final String OTP_RING0_FUNCTION = "start";
    private static final String RESOURCES_PATH = "resources/";
    private static final String INTERNAL_AST_FILE_NAME_PREFIX = "internal:";

    private static final List<Source> internalSources;

    static {
        internalSources = new ArrayList<>();

        try {
            URI uri = ErlangLanguage.class.getResource(RESOURCES_PATH).toURI();
            Path resPath;
            if (uri.getScheme().equals("jar")) {
                FileSystem fileSystem = FileSystems.newFileSystem(uri, Collections.<String, Object> emptyMap());
                resPath = fileSystem.getPath("/resources");
            } else {
                resPath = Paths.get(uri);
            }
            Stream<Path> walk = Files.walk(resPath);
            for (Iterator<Path> it = walk.iterator(); it.hasNext();) {
                Path p = it.next();
                if (p.toString().endsWith(".ast")) {
                    internalSources.add(Source.fromReader(new InputStreamReader(p.toUri().toURL().openStream()), INTERNAL_AST_FILE_NAME_PREFIX + p));
                }
            }
        } catch (IOException | URISyntaxException ex) {
            // ignore
        }
    }

    @Override
    protected ErlContext createContext(Env env) {
        final BufferedReader in = new BufferedReader(new InputStreamReader(env.in()));
        final PrintWriter out = new PrintWriter(env.out(), true);
        ErlContext erlContext = new ErlContext(this, in, out);
        for (NodeFactory<? extends ErlBuiltinNode> builtin : builtins) {
            erlContext.installBuiltin(builtin, true);
        }

        for (Source source : internalSources) {
            erlContext.evalPreprocessed(source, true);
            // erlContext.getModuleRegistry().put(source.getShortName(), module);
        }
        return erlContext;
    }

    private static ErlList buildInitArgs(String[] args) {
        ArrayList<Object> list = new ArrayList<>();

        list.add(ErlContext.stringToBinary("-root", ErlContext.LATIN1_CHARSET));
        list.add(ErlContext.stringToBinary("/usr/lib/erlang", ErlContext.LATIN1_CHARSET));

        for (String arg : args) {
            list.add(ErlContext.stringToBinary(arg, ErlContext.LATIN1_CHARSET));
        }

        return ErlList.fromList(list);
    }

    @Override
    protected void disposeContext(ErlContext erlContext) {
        erlContext.close();
    }

    public static ErlContext getContext(PolyglotEngine engine) {
        try {
            return engine.getLanguages().get(ERL_MIME_TYPE).getGlobalObject().as(ErlContext.class);
        } catch (IOException e) {
            return null;
        }
    }

    /**
     * The main entry point. Use the mx command <code>mx erl</code> to run it with the correct class
     * path setup.
     */
    public static void main(String[] args) throws IOException {
        PolyglotEngine engine = PolyglotEngine.newBuilder().build();
        assert engine.getLanguages().containsKey(ERL_MIME_TYPE);

        final ErlContext context = getContext(engine);

        if (args.length > 0 && "-independent".equals(args[0])) {
            boolean gotFile = false;
            int index = 1;
            String moduleName = null;
            String functionName = null;

            while (index < args.length && !"--".equals(args[index])) {
                if ("-file".equals(args[index])) {
                    final String filename = args[index + 1];
                    index += 2;

                    final Source source = Source.fromFileName(filename);
                    engine.eval(source);

                    gotFile = true;

                } else if ("-mf".equals(args[index])) {
                    moduleName = args[index + 1];
                    functionName = args[index + 2];
                    index += 3;

                } else {
                    throw new ErlException("Unknown switch: " + args[index] + ".");
                }
            }

            if (null == moduleName) {
                throw new ErlException("Function is not set.");
            }

            if (!gotFile) {
                final Source source = Source.fromReader(new InputStreamReader(System.in), "<stdin>").withMimeType(ERL_MIME_TYPE);
                engine.eval(source);
            }

            if (index != args.length) {
                // TODO
                throw new ErlException("Argument passing is not implemented yet.");
            }

            final Object[] arguments = new Object[0];

            final ErlModule module = context.getModuleRegistry().getModule(moduleName);
            if (null == module) {
                throw new ErlException("Module " + moduleName + " is not loaded.");
            }

            final Future<Object> future = module.start(context, functionName, arguments);
            if (null == future) {
                throw new ErlException("Function " + moduleName + ":" + functionName + "/" + arguments.length + " is not defined.");
            }

            System.out.println(stringifyResult(future));

        } else {

            final ErlModule module = context.getModuleRegistry().getModule(OTP_RING0_MODULE);
            if (null == module.start(context, "start", ErlList.NIL, buildInitArgs(args))) {
                throw new RuntimeException("Function " + OTP_RING0_MODULE + ":" + OTP_RING0_FUNCTION + " function not found");
            }
            context.waitForTerminateAll();
        }

        engine.dispose();
    }

    private static String stringifyResult(Future<Object> result) {
        try {

            Object obj = result.get();
            return "" + obj;

        } catch (CancellationException | InterruptedException | ExecutionException e) {

            if (e.getCause() instanceof ErlControlException) {
                return "** exception: " + ((ErlControlException) e.getCause()).getDescribingTerm();
            } else {
                return "** " + ((e instanceof CancellationException) ? "cancelled" : e.getCause());
            }
        }
    }

    /**
     * Parse and run the specified SL source. Factored out in a separate method so that it can also
     * be used by the unit test harness.
     */
    public static long run(PolyglotEngine engine, Path path, PrintWriter logOutput, PrintWriter out, int repeats, List<NodeFactory<? extends ErlBuiltinNode>> currentBuiltins) throws IOException {
        builtins = currentBuiltins;

        if (logOutput != null) {
            logOutput.println("== running on " + Truffle.getRuntime().getName());
            // logOutput.println("Source = " + source.getCode());
        }

        Source src = Source.fromFileName(path.toString());
        /* Parse the Erlang source file. */
        Object result = engine.eval(src.withMimeType(ERL_MIME_TYPE)).get();
        // if (result != null) {
        // out.println(result);
        // }

        final ErlModule module = (ErlModule) result;
        final String functionName = "main";

        /* Lookup our main entry point, which is per definition always named "main". */
        if (!module.functionExists(functionName, 0)) {
            throw new ErlException("No function main() defined in Erlang source file.");
        }

        /* Change to true if you want to see the AST on the console. */
        boolean printASTToLog = false;
        /* Change to true if you want to see source attribution for the AST to the console */
        boolean printSourceAttributionToLog = false;
        /* Change to dump the AST to IGV over the network. */
        boolean dumpASTToIGV = false;

        printScript("before execution", null, logOutput, printASTToLog, printSourceAttributionToLog, dumpASTToIGV);
        long totalRuntime = 0;
        try {
            final ErlContext context = getContext(engine);

            for (int i = 0; i < repeats; i++) {
                long start = System.nanoTime();
                /* Call the main entry point, without any arguments. */
                try {
                    Future<Object> future = module.start(context, functionName);
                    out.println(stringifyResult(future));
                } catch (UnsupportedSpecializationException ex) {
                    out.println(formatTypeError(ex));
                } catch (ErlUndefinedFunctionException ex) {
                    out.println(String.format("Undefined function: %s", ex.getFunctionName()));
                }
                long end = System.nanoTime();
                totalRuntime += end - start;

                if (logOutput != null && repeats > 1) {
                    logOutput.println("== iteration " + (i + 1) + ": " + ((end - start) / 1000000) + " ms");
                }
            }

        } finally {
            printScript("after execution", null, logOutput, printASTToLog, printSourceAttributionToLog, dumpASTToIGV);
        }
        return totalRuntime;
    }

    /**
     * Provides a user-readable message for run-time type errors. SL is strongly typed, i.e., there
     * are no automatic type conversions of values. Therefore, Truffle does the type checking for
     * us: if no matching node specialization for the actual values is found, then we have a type
     * error. Specialized nodes use the {@link UnsupportedSpecializationException} to report that no
     * specialization was found. We therefore just have to convert the information encapsulated in
     * this exception in a user-readable form.
     */
    private static String formatTypeError(UnsupportedSpecializationException ex) {
        StringBuilder result = new StringBuilder();
        result.append("Type error");
        if (ex.getNode() != null && ex.getNode().getSourceSection() != null) {
            SourceSection ss = ex.getNode().getSourceSection();
            if (ss != null && ss.getSource() != null) {
                result.append(" at ").append(ss.getSource().getShortName()).append(" line ").append(ss.getStartLine()).append(" col ").append(ss.getStartColumn());
            }
        }
        result.append(": operation");
        if (ex.getNode() != null) {
            NodeInfo nodeInfo = ErlContext.lookupNodeInfo(ex.getNode().getClass());
            if (nodeInfo != null) {
                result.append(" \"").append(nodeInfo.shortName()).append("\"");
            }
        }
        result.append(" not defined for");

        String sep = " ";
        for (int i = 0; i < ex.getSuppliedValues().length; i++) {
            Object value = ex.getSuppliedValues()[i];
            Node node = ex.getSuppliedNodes()[i];
            if (node != null) {
                result.append(sep);
                sep = ", ";

                if (value instanceof Long || value instanceof BigInteger) {
                    result.append("Number ").append(value);
                } else if (value instanceof Boolean) {
                    result.append("Boolean ").append(value);
                } else if (value instanceof String) {
                    result.append("String \"").append(value).append("\"");
                } else if (value instanceof ErlFunction) {
                    result.append("Function ").append(value);
                } else if (value instanceof ErlAtom) {
                    result.append("Atom ").append(value);
                } else if (value == null) {
                    // value is not evaluated because of short circuit evaluation
                    result.append("ANY");
                } else {
                    result.append(value);
                }
            }
        }
        return result.toString();
    }

    /**
     * When dumpASTToIGV is true: dumps the AST of all functions to the IGV visualizer, via a socket
     * connection. IGV can be started with the mx command "mx igv".
     * <p>
     * When printASTToLog is true: prints the ASTs to the console.
     */
    private static void printScript(String groupName, ErlContext context, PrintWriter logOutput, boolean printASTToLog, boolean printSourceAttributionToLog, boolean dumpASTToIGV) {
        if (dumpASTToIGV) {
            GraphPrintVisitor graphPrinter = new GraphPrintVisitor();
            graphPrinter.beginGroup(groupName);
            for (ErlFunction function : context.getModuleRegistry().getFunctions()) {
                RootCallTarget callTarget = function.getCallTarget();
                if (callTarget != null) {
                    graphPrinter.beginGraph(function.toString()).visit(callTarget.getRootNode());
                }
            }
            graphPrinter.printToNetwork(true);
        }
        if (printASTToLog && logOutput != null) {
            for (ErlFunction function : context.getModuleRegistry().getFunctions()) {
                RootCallTarget callTarget = function.getCallTarget();
                if (callTarget != null) {
                    logOutput.println("=== " + function);
                    NodeUtil.printTree(logOutput, callTarget.getRootNode());
                }
            }
        }
        if (printSourceAttributionToLog && logOutput != null) {
            for (ErlFunction function : context.getModuleRegistry().getFunctions()) {
                RootCallTarget callTarget = function.getCallTarget();
                if (callTarget != null) {
                    logOutput.println("=== " + function);
                    NodeUtil.printSourceAttributionTree(logOutput, callTarget.getRootNode());
                }
            }
        }
    }

    @Override
    protected CallTarget parse(Source code, Node node, String... argumentNames) throws IOException {
        final ErlContext c = new ErlContext(this);

        final Exception[] failed = {null};
        try {
            c.evalSource(code);
            failed[0] = null;
        } catch (Exception e) {
            failed[0] = e;
        }
        return new CallTarget() {
            @Override
            public Object call(Object... arguments) {
                if (failed[0] instanceof RuntimeException) {
                    throw (RuntimeException) failed[0];
                }
                if (failed[0] != null) {
                    throw new IllegalStateException(failed[0]);
                }
                Node n = createFindContextNode();
                ErlContext fillIn = findContext(n);
                final ErlModuleRegistry moduleRegistry = fillIn.getModuleRegistry();
                ErlModuleImpl loadedModule = null;
                for (ErlModuleImpl mod : c.getModuleRegistry().getModules()) {
                    if (mod.isPreLoaded()) {
                        continue;
                    }
                    assert null == loadedModule;
                    moduleRegistry.register(mod);
                    loadedModule = mod;
                }
                return loadedModule;
            }
        };
    }

    @Override
    protected Object findExportedSymbol(ErlContext context_, String globalName, boolean onlyExplicit) {

        // for (ErlFunction f : context_.getFunctionRegistry().getFunctions()) {
        // if (globalName.equals(f.getName()) || globalName.equals(f.getModule() + ":" +
        // f.getName())) {
        // return f;
        // }
        // }

        return null;
    }

    @Override
    protected ErlContext getLanguageGlobal(ErlContext context_) {
        return context_;
    }

    @Override
    protected boolean isObjectOfLanguage(Object object) {
        return object instanceof ErlFunction || object instanceof ErlAtom || object instanceof ErlBinary || object instanceof ErlLazyBinary || object instanceof ErlBinaryView ||
                        object instanceof ErlList || object instanceof ErlPort || object instanceof ErlPid || object instanceof ErlRef || object instanceof ErlTuple;
    }

    @Override
    protected Visualizer getVisualizer() {
        if (visualizer == null) {
            visualizer = new ErlDefaultVisualizer();
        }
        return visualizer;
    }

    @Override
    protected boolean isInstrumentable(Node node) {
        return node instanceof ErlExpressionNode;
    }

    @Override
    protected WrapperNode createWrapperNode(Node node) {
        if (node instanceof ErlExpressionNode) {
            return new ErlExpressionWrapperNode((ErlExpressionNode) node);
        }
        return null;
    }

    @Override
    protected Object evalInContext(Source source, Node node, MaterializedFrame mFrame) throws IOException {
        throw new IllegalStateException("evalInContext not supported in this language");
    }
}
