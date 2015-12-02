/*
 * Copyright (c) 2014, Oracle and/or its affiliates. All rights reserved. DO NOT ALTER OR REMOVE
 * COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * The Universal Permissive License (UPL), Version 1.0
 *
 * Subject to the condition set forth below, permission is hereby granted to any person obtaining a
 * copy of this software, associated documentation and/or data (collectively the "Software"), free
 * of charge and under any and all copyright rights in the Software, and any and all patent rights
 * owned or freely licensable by each licensor hereunder covering either (i) the unmodified Software
 * as contributed to or provided by such licensor, or (ii) the Larger Works (as defined below), to
 * deal in both
 *
 * (a) the Software, and
 *
 * (b) any piece of software and/or hardware listed in the lrgrwrks.txt file if one is included with
 * the Software each a "Larger Work" to which the Software is contributed by such licensors),
 *
 * without restriction, including without limitation the rights to copy, create derivative works of,
 * display, perform, and distribute the Software and make, use, sell, offer for sale, import,
 * export, have made, and have sold the Software and the Larger Work(s), and to sublicense the
 * foregoing rights on either these or other terms.
 *
 * This license is subject to the following condition:
 *
 * The above copyright notice and either this complete permission notice or at a minimum a reference
 * to the UPL must be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package com.oracle.truffle.erl.parser;

import java.io.BufferedReader;
import java.io.IOException;
import java.math.BigInteger;
import java.nio.CharBuffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.FrameSlot;
import com.oracle.truffle.erl.FA;
import com.oracle.truffle.erl.MFA;
import com.oracle.truffle.erl.nodes.ErlExpressionNode;
import com.oracle.truffle.erl.nodes.ErlRootNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlClauseSelectorNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlFunctionBodyNode;
import com.oracle.truffle.erl.nodes.expression.ErlToBooleanNodeGen;
import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlFunction;
import com.oracle.truffle.erl.runtime.ErlModuleImpl;
import com.oracle.truffle.erl.nodes.call.ErlInvokeNodeGen;
import com.oracle.truffle.erl.nodes.controlflow.ErlBinComprehensionNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlBinGeneratorNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlBlockNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlCaseNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlCatchNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlClauseNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlIfNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlListComprehensionNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlListGeneratorNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlReceiveNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlTryNode;
import com.oracle.truffle.erl.nodes.expression.ErlAddNodeGen;
import com.oracle.truffle.erl.nodes.expression.ErlAtomLiteralNode;
import com.oracle.truffle.erl.nodes.expression.ErlBigIntegerLiteralNode;
import com.oracle.truffle.erl.nodes.expression.ErlBinElementNode;
import com.oracle.truffle.erl.nodes.expression.ErlBinNode;
import com.oracle.truffle.erl.nodes.expression.ErlBitshiftLeftNodeGen;
import com.oracle.truffle.erl.nodes.expression.ErlBitshiftRightNodeGen;
import com.oracle.truffle.erl.nodes.expression.ErlBitwiseAndNodeGen;
import com.oracle.truffle.erl.nodes.expression.ErlBitwiseOrNodeGen;
import com.oracle.truffle.erl.nodes.expression.ErlBitwiseXorNodeGen;
import com.oracle.truffle.erl.nodes.expression.ErlCaptureNode;
import com.oracle.truffle.erl.nodes.expression.ErlDoubleLiteralNode;
import com.oracle.truffle.erl.nodes.expression.ErlDynamicFunctionNode;
import com.oracle.truffle.erl.nodes.expression.ErlEqualNodeGen;
import com.oracle.truffle.erl.nodes.expression.ErlExactEqualNodeGen;
import com.oracle.truffle.erl.nodes.expression.ErlFloatDivideNodeGen;
import com.oracle.truffle.erl.nodes.expression.ErlFunctionLiteralNode;
import com.oracle.truffle.erl.nodes.expression.ErlFunctionRefNode;
import com.oracle.truffle.erl.nodes.expression.ErlIntegerDivideNodeGen;
import com.oracle.truffle.erl.nodes.expression.ErlIntegerRemainderNodeGen;
import com.oracle.truffle.erl.nodes.expression.ErlLessThanNodeGen;
import com.oracle.truffle.erl.nodes.expression.ErlListAddNode;
import com.oracle.truffle.erl.nodes.expression.ErlListConsNode;
import com.oracle.truffle.erl.nodes.expression.ErlListNilNode;
import com.oracle.truffle.erl.nodes.expression.ErlListSubtractNodeGen;
import com.oracle.truffle.erl.nodes.expression.ErlLogicalAndNodeGen;
import com.oracle.truffle.erl.nodes.expression.ErlLogicalNotNodeGen;
import com.oracle.truffle.erl.nodes.expression.ErlLogicalOrNodeGen;
import com.oracle.truffle.erl.nodes.expression.ErlLogicalXorNodeGen;
import com.oracle.truffle.erl.nodes.expression.ErlLongLiteralNode;
import com.oracle.truffle.erl.nodes.expression.ErlMapFieldAssocNode;
import com.oracle.truffle.erl.nodes.expression.ErlMapFieldExactNode;
import com.oracle.truffle.erl.nodes.expression.ErlMapNode;
import com.oracle.truffle.erl.nodes.expression.ErlMatchNode;
import com.oracle.truffle.erl.nodes.expression.ErlMultiplyNodeGen;
import com.oracle.truffle.erl.nodes.expression.ErlSendNodeGen;
import com.oracle.truffle.erl.nodes.expression.ErlSubtractNodeGen;
import com.oracle.truffle.erl.nodes.expression.ErlTupleNode;
import com.oracle.truffle.erl.nodes.expression.ErlUnaryBitwiseNotNodeGen;
import com.oracle.truffle.erl.nodes.expression.ErlUnaryNegateNodeGen;
import com.oracle.truffle.erl.nodes.local.ErlBindContextVariableNode;
import com.oracle.truffle.erl.nodes.local.ErlBindContextVariableNodeGen;
import com.oracle.truffle.erl.nodes.local.ErlLocalVariableNode;
import com.oracle.truffle.erl.nodes.local.ErlLocalVariableNodeGen;

class ErlAstParser {

    private final BufferedReader br;
    private final ErlModuleImpl module;
    private final String moduleName;
    private static final char SEPARATOR = '\uAAAA';
    private char preread;
    private final HashMap<FA, String> importedFunctions = new HashMap<>();
    private StackableSet<String> boundVariables = new StackableSet<>();

    ErlAstParser(BufferedReader br, ErlModuleImpl module) throws IOException {
        this.br = br;
        this.module = module;
        this.moduleName = module.getModuleName();

        loadImports(br.readLine());
        loadOnLoads(br.readLine());
        readNext();
    }

    private void loadImports(String imports) {
        CharBuffer cb = CharBuffer.wrap(imports);
        accept('[', cb);

        if (nextIs('{', cb)) {

            boolean again = false;

            do {
                accept('{', cb);
                final String targetModule = readUntil(',', cb);
                accept(',', cb);
                final String func = readUntil(',', cb);
                accept(',', cb);
                final String arity = readUntil('}', cb);
                accept('}', cb);

                importedFunctions.put(new FA(func, Integer.valueOf(arity)), targetModule);

                again = nextIs(',', cb);
                if (again) {
                    accept(',', cb);
                }

            } while (again);
        }

        accept(']', cb);
    }

    private void loadOnLoads(String imports) {
        CharBuffer cb = CharBuffer.wrap(imports);
        accept('[', cb);

        if (nextIs('{', cb)) {

            boolean again = false;

            do {
                accept('{', cb);
                final String func = readUntil(',', cb);
                accept(',', cb);
                final String arityStr = readUntil('}', cb);
                final int arity = Integer.valueOf(arityStr);
                accept('}', cb);

                if (0 != arity) {
                    throw new RuntimeException("Arity of the onload function must be 0.");
                }

                module.addOnLoadFunction(new FA(func, arity));

                again = nextIs(',', cb);
                if (again) {
                    accept(',', cb);
                }

            } while (again);
        }

        accept(']', cb);
    }

    private static String readUntil(char term, CharBuffer cb) {
        StringBuilder sb = new StringBuilder();
        while (!nextIs(term, cb)) {
            sb.append(cb.get());
        }

        return sb.toString();
    }

    private static boolean accept(char ch, CharBuffer cb) {
        if (nextIs(ch, cb)) {
            cb.get();
            return true;
        }

        throw new RuntimeException("Cannot accept '" + ch + "' while parsing imports.");
    }

    private static boolean nextIs(char ch, CharBuffer cb) {
        return cb.hasRemaining() && ch == cb.charAt(0);
    }

    private long nextUniqueId = 1;

    private String generateUniqueId() {
        return "" + (nextUniqueId++);
    }

    private String generateUniqueName() {
        return "_" + SEPARATOR + generateUniqueId();
    }

    private void readNext() {
        try {
            do {
                preread = (char) br.read();
            } while (Character.isWhitespace(preread));
        } catch (IOException e) {
            preread = '\0';
        }
    }

    private void readNextRaw() {
        try {
            preread = (char) br.read();
        } catch (IOException e) {
            preread = '\0';
        }
    }

    private boolean accept(final char ch) {
        if (ch != preread) {
            throw new RuntimeException("Failed to reconstruct Erlang AST. Cannot accept char '" + ch + "', because '" + preread + "' was read.");
        }

        readNext();
        return true;
    }

    private boolean weakAccept(final char ch) {
        final boolean result = (ch == preread);
        if (result) {
            readNext();
        }
        return result;
    }

    private boolean nextIs(final char ch) {
        return ch == preread;
    }

    private void acceptAtom(final String expectedAtom) {
        final String actualAtom = parseAtom();
        if (!actualAtom.equals(expectedAtom)) {
            throw new RuntimeException("Failed to reconstruct Erlang AST. Cannot accept atom '" + expectedAtom + "', because '" + actualAtom + "' was read.");
        }
    }

    private long parseInteger() {

        long result = 0;
        boolean negative = false;

        if ('-' == preread) {
            negative = true;
            readNext();
        }

        while ('0' <= preread && preread <= '9') {
            result = (result * 10) + (preread - '0');
            readNext();
        }

        if (negative) {
            return -result;
        }

        return result;
    }

    private String parseRawInteger() {

        String raw = "";

        do {
            raw = raw + preread;
            readNext();
        } while ('0' <= preread && preread <= '9');

        return raw;
    }

    private String parseRawFloat() {

        String raw = "";

        do {
            raw = raw + preread;
            readNext();
        } while ('0' <= preread && preread <= '9' || '.' == preread || 'e' == preread || '-' == preread);

        return raw;
    }

    private String parseAtom() {

        String result = "";
        char prev = 0;

        if ('\'' == preread) {
            result = result + "\'";
            readNextRaw();
            while ('\\' == prev || '\'' != preread) {
                if ('\\' == prev && '\\' == preread) {
                    prev = 0;
                } else {
                    prev = preread;
                }
                result = result + preread;
                readNextRaw();
            }
            result = result + "\'";
            accept('\'');

            result = ErlAtom.unescapeAtom(result);

        } else {
            while (Character.isLetterOrDigit(preread) || '_' == preread || '@' == preread) {
                result = result + preread;
                readNext();
            }
        }

        return result;
    }

    private String parseString() {
        accept('\"');

        String result = "";
        char prev = 0;

        while ('\\' == prev || '\"' != preread) {
            if ('\\' == prev && '\\' == preread) {
                prev = 0;
            } else {
                prev = preread;
            }
            result = result + preread;
            readNextRaw();
        }

        accept('\"');

        return result;
    }

    public void parse() {
        accept('[');
        do {

            parseFunction();

        } while (weakAccept(','));
        accept(']');
    }

    private static ErlExpressionNode toBoolean(ErlExpressionNode expr) {
        return ErlToBooleanNodeGen.create(null, expr);
    }

    private ErlFunction parseFunction() {

        boundVariables.push();

        accept('{');
        acceptAtom("function");
        accept(',');
        parseInteger(); // ignore the line number
        accept(',');
        final String name = parseAtom();
        accept(',');
        final int arity = (int) parseInteger();
        assert arity >= 0;
        accept(',');

        int refArity[] = new int[]{arity};

        FrameDescriptor fd = new FrameDescriptor();
        ErlClauseSelectorNode selector = parseClauseList(fd, refArity, false);
        accept('}');

        ErlFunctionBodyNode bodyNode = new ErlFunctionBodyNode(null, selector);
        bodyNode.markAsTail();
        ErlRootNode rootNode = new ErlRootNode(fd, bodyNode, new MFA(moduleName, name, arity));

        boundVariables.pop();

        return module.register(name, arity, rootNode, ErlFunction.Origin.REGULAR);
    }

    private ErlExpressionNode parseClausesAsFunction(FrameDescriptor fd0) {

        boundVariables.push();

        StackableSet.CollectAccessOf<String> accessOf = new StackableSet.CollectAccessOf<>(boundVariables.get());
        boundVariables.monitorAdd(accessOf);

        int arity = ErlFunction.UNRESOLVED_ARITY;
        accept(',');

        int refArity[] = new int[]{arity};

        FrameDescriptor fd = fd0.shallowCopy();
        ErlClauseSelectorNode selector = parseClauseList(fd, refArity, false);
        accept('}');

        boundVariables.monitorRemove(accessOf);

        // correct the arity
        arity = refArity[0];
        final String name = "-fun-anon-" + generateUniqueId() + "-" + arity + "-";

        final ErlExpressionNode preludeNode = createPreludeNode(accessOf.access, fd);
        final ErlFunctionBodyNode bodyNode = new ErlFunctionBodyNode(null, preludeNode, selector);
        bodyNode.markAsTail();
        final ErlRootNode rootNode = new ErlRootNode(fd, bodyNode, new MFA(moduleName, name, arity));

        boundVariables.pop();

        module.register(name, arity, rootNode, ErlFunction.Origin.ANONYMOUS);
        final ErlFunctionLiteralNode funclitNode = new ErlFunctionLiteralNode(null, moduleName, name, arity);
        return createCaptureNode(funclitNode, accessOf.access, fd0);
    }

    private static ErlExpressionNode createPreludeNode(HashSet<String> access, FrameDescriptor fd, ErlExpressionNode... otherNodes) {

        ArrayList<ErlExpressionNode> nodes = new ArrayList<>();
        int contextArgumentId = 0;

        for (String name : access) {
            final FrameSlot slot = fd.findOrAddFrameSlot(name);
            final ErlBindContextVariableNode node = ErlBindContextVariableNodeGen.create(null, contextArgumentId++, slot);
            nodes.add(node);
        }

        for (ErlExpressionNode node : otherNodes) {
            nodes.add(node);
        }

        if (nodes.isEmpty()) {
            return null;
        } else if (1 == nodes.size()) {
            return nodes.get(0);
        } else {
            return new ErlBlockNode(null, nodes.toArray(new ErlExpressionNode[nodes.size()]));
        }
    }

    private static ErlExpressionNode createCaptureNode(ErlFunctionLiteralNode funclitNode, HashSet<String> access, FrameDescriptor fd) {

        ArrayList<ErlExpressionNode> nodes = new ArrayList<>();

        for (String name : access) {

            final FrameSlot slot = fd.findFrameSlot(name);
            final ErlLocalVariableNode node = ErlLocalVariableNodeGen.create(null, slot);
            nodes.add(node);
        }

        if (nodes.isEmpty()) {
            return funclitNode;
        } else {
            return new ErlCaptureNode(null, funclitNode, nodes.toArray(new ErlExpressionNode[nodes.size()]));
        }
    }

    private ErlClauseNode parseClause(FrameDescriptor fd, int[] refArity) {

        assert null != fd;

        boundVariables.push();

        accept('{');
        acceptAtom("clause");
        accept(',');
        parseInteger(); // ignore line number
        accept(',');
        List<ErlExpressionNode> matchNodes = parseClauseArguments(fd, refArity);
        accept(',');
        ErlExpressionNode conditionNode = parseGuardSeq(fd);
        accept(',');
        boundVariables.pushAcceptAll();
        List<ErlExpressionNode> exprs = parseExpressions(fd, true);
        boundVariables.popAcceptAll();
        accept('}');

        boundVariables.pop();

        return new ErlClauseNode(null, matchNodes.toArray(new ErlExpressionNode[matchNodes.size()]), conditionNode, exprs.toArray(new ErlExpressionNode[exprs.size()]));
    }

    private ErlClauseSelectorNode parseClauseList(FrameDescriptor fd, int[] refArity, boolean canBeEmpty) {

        ArrayList<ErlClauseNode> clauses = new ArrayList<>();

        accept('[');

        if (!weakAccept(']')) {

            do {
                clauses.add(parseClause(fd, refArity));
            } while (weakAccept(','));

            accept(']');
        }

        if (clauses.isEmpty()) {
            assert canBeEmpty;
            return null;
        }

        return new ErlClauseSelectorNode(null, clauses.toArray(new ErlClauseNode[clauses.size()]));
    }

    private List<ErlExpressionNode> parseClauseArguments(FrameDescriptor fd, int[] refArity) {

        ArrayList<ErlExpressionNode> matchNodes = new ArrayList<>();

        accept('[');

        if (!weakAccept(']')) {

            do {
                matchNodes.add(parseArgument(fd));
            } while (weakAccept(','));

            accept(']');
        }

        final int arg_count = matchNodes.size();

        // arity and the number of read arguments MUST match
        assert ErlFunction.UNRESOLVED_ARITY == refArity[0] || refArity[0] == arg_count;
        refArity[0] = arg_count;

        return matchNodes;
    }

    private ErlExpressionNode parseArgument(FrameDescriptor fd) {

        return parseExpression(fd);
    }

    private ErlExpressionNode parseGuardSeq(FrameDescriptor fd) {

        accept('[');

        LinkedList<ErlExpressionNode> disjunctions = new LinkedList<>();

        if (nextIs('[')) {

            do {
                accept('[');

                LinkedList<ErlExpressionNode> conjunctions = new LinkedList<>();

                conjunctions.add(toBoolean(parseExpression(fd)));

                while (weakAccept(',')) {
                    conjunctions.add(ErlToBooleanNodeGen.create(null, parseExpression(fd)));
                }

                accept(']');

                // create an unbalanced tree from all the conditions in order to stop the
                // evaluation
                // as soon as possible (if one of the conditions is false)

                ErlExpressionNode theConjunction = conjunctions.getLast();
                conjunctions.removeLast();

                while (!conjunctions.isEmpty()) {
                    theConjunction = ErlLogicalAndNodeGen.create(null, conjunctions.getLast(), theConjunction);
                    conjunctions.removeLast();
                }

                disjunctions.add(theConjunction);

            } while (weakAccept(','));
        }

        accept(']');

        if (!disjunctions.isEmpty()) {

            ErlExpressionNode disjunction = disjunctions.getLast();
            disjunctions.removeLast();

            while (!disjunctions.isEmpty()) {
                disjunction = ErlLogicalOrNodeGen.create(null, disjunctions.getLast(), disjunction);
                disjunctions.removeLast();
            }

            return disjunction;
        }

        return null;
    }

    private List<ErlExpressionNode> parseExpressions(FrameDescriptor fd, boolean atLeastOne) {

        List<ErlExpressionNode> exprs = new ArrayList<>();

        accept('[');

        if (atLeastOne || !weakAccept(']')) {

            do {
                try {
                    exprs.add(parseExpression(fd));
                } catch (PackedBinElementException ex) {
                    while (ex.hasNext()) {
                        exprs.add(ex.next());
                    }
                }
            } while (weakAccept(','));

            accept(']');
        }

        return exprs;
    }

    private ErlExpressionNode[] parseExpressionArray(FrameDescriptor fd, boolean atLeastOne) {

        List<ErlExpressionNode> exprs = parseExpressions(fd, atLeastOne);
        return exprs.toArray(new ErlExpressionNode[exprs.size()]);
    }

    private ErlExpressionNode parseExpressionBlock(FrameDescriptor fd, boolean atLeastOne) {

        List<ErlExpressionNode> exprs = parseExpressions(fd, atLeastOne);

        if (exprs.isEmpty()) {
            return null;
        } else if (1 == exprs.size()) {
            return exprs.get(0);
        } else {
            return new ErlBlockNode(null, exprs.toArray(new ErlExpressionNode[exprs.size()]));
        }
    }

    private ErlExpressionNode parseExpression(FrameDescriptor fd) {

        ErlExpressionNode expr = null;

        accept('{');
        final String kind = parseAtom();
        accept(',');

        switch (kind) {
            case "var": {
                parseInteger(); // ignore line number
                accept(',');

                String name = parseAtom();

                if ("_".equals(name)) {
                    name = generateUniqueName();
                }

                final boolean bound = !boundVariables.add(name);

                FrameSlot slot = fd.findOrAddFrameSlot(name);

                if (bound) {

                    // bound variable: just make the variable access to bound

                    ErlLocalVariableNode var = ErlLocalVariableNodeGen.create(null, slot);
                    var.setBound();

                    expr = var;

                } else {

                    // unbound variable

                    expr = ErlLocalVariableNodeGen.create(null, slot);
                }
                break;
            }

            case "op": {
                parseInteger(); // ignore line number
                accept(',');

                final String op = parseAtom();
                accept(',');
                final ErlExpressionNode lhs = parseExpression(fd);
                // we implement only unary and binary operators
                if (weakAccept(',')) {
                    final ErlExpressionNode rhs = parseExpression(fd);
                    expr = genBinaryOperator(op, lhs, rhs);
                } else {
                    expr = genUnaryOperator(op, lhs);
                }
                break;
            }

            case "atom": {
                parseInteger(); // ignore line number
                accept(',');

                expr = new ErlAtomLiteralNode(null, new ErlAtom(parseAtom()));
                break;
            }

            case "integer": {
                parseInteger(); // ignore line number
                accept(',');

                final String raw = parseRawInteger();
                try {
                    expr = new ErlLongLiteralNode(null, Long.parseLong(raw));
                } catch (NumberFormatException ex) {
                    expr = new ErlBigIntegerLiteralNode(null, new BigInteger(raw));
                }
                break;
            }

            case "float": {
                parseInteger(); // ignore line number
                accept(',');

                final String raw = parseRawFloat();
                expr = new ErlDoubleLiteralNode(null, Double.parseDouble(raw));
                break;
            }

            case "nil": {
                parseInteger(); // ignore line number
                expr = new ErlListNilNode(null);
                break;
            }

            case "cons": {
                parseInteger(); // ignore line number
                accept(',');
                final ErlExpressionNode headNode = parseExpression(fd);
                accept(',');
                final ErlExpressionNode tailNode = parseExpression(fd);
                expr = new ErlListConsNode(null, headNode, tailNode);
                break;
            }

            case "string": {
                parseInteger(); // ignore line number
                accept(',');

                if (weakAccept('[')) {

                    if (weakAccept(']')) {
                        // TODO
                        // expr = new ErlStringConsNode(null, "");
                        expr = new ErlListNilNode(null);
                    } else {
                        LinkedList<Long> chars = new LinkedList<>();

                        do {
                            chars.push(parseInteger());
                        } while (weakAccept(','));

                        accept(']');

                        expr = new ErlListNilNode(null);

                        while (!chars.isEmpty()) {
                            expr = new ErlListConsNode(null, new ErlLongLiteralNode(null, chars.pop()), expr);
                        }
                    }
                } else {
                    // TODO
                    // expr = new ErlStringConsNode(null, parseString());
                    expr = decomposeStringToListCons(parseString());
                }

                break;
            }

            case "char": {
                parseInteger(); // ignore line number
                accept(',');

                expr = new ErlLongLiteralNode(null, parseInteger());
                break;
            }

            case "fun": {
                expr = parseFun(fd);
                break;
            }

            case "named_fun": {
                expr = parseNamedFun(fd);
                break;
            }

            case "call": {
                parseInteger(); // ignore line number
                accept(',');

                final ErlExpressionNode funcExpr = parseExpression(fd);
                accept(',');
                final ErlExpressionNode args[] = parseExpressionArray(fd, false);

                if (funcExpr instanceof ErlAtomLiteralNode) {

                    final String calleeName = ((ErlAtomLiteralNode) funcExpr).getValue().getValue();
                    final FA fa = new FA(calleeName, args.length);
                    final String importedModule = importedFunctions.get(fa);

                    if (null != importedModule) {

                        final ErlExpressionNode moduleNameNode = new ErlAtomLiteralNode(null, new ErlAtom(importedModule));
                        final ErlExpressionNode arityNode = new ErlLongLiteralNode(null, args.length);
                        final ErlDynamicFunctionNode dynFunc = new ErlDynamicFunctionNode(null, moduleNameNode, funcExpr, arityNode);

                        expr = ErlInvokeNodeGen.create(null, args, dynFunc);

                    } else {

                        expr = ErlInvokeNodeGen.create(null, args, new ErlFunctionLiteralNode(null, moduleName, calleeName, args.length));
                    }

                } else if (funcExpr instanceof ErlFunctionLiteralNode) {

                    ErlFunctionLiteralNode funcLit = (ErlFunctionLiteralNode) funcExpr;

                    if (funcLit.getArity() == ErlFunction.UNRESOLVED_ARITY) {

                        funcLit = new ErlFunctionLiteralNode(funcLit.getSourceSection(), funcLit.getModuleName(), funcLit.getFuncName(), args.length);
                    }

                    expr = ErlInvokeNodeGen.create(null, args, funcLit);

                } else if (funcExpr instanceof ErlDynamicFunctionNode) {

                    ErlDynamicFunctionNode dynFunc = (ErlDynamicFunctionNode) funcExpr;

                    expr = ErlInvokeNodeGen.create(null, args, dynFunc.withArity(args.length));

                } else {

                    // 'funcExpr' is a generic expression, so we need to wrap into an
                    // ErlFunctionRefNode to ensure that the result of the 'funcExpr' is really
                    // a function
                    expr = ErlInvokeNodeGen.create(null, args, new ErlFunctionRefNode(null, funcExpr));
                }
                break;
            }

            case "remote": {
                parseInteger(); // ignore line number
                accept(',');

                final ErlExpressionNode moduleNode = parseExpression(fd);
                accept(',');
                final ErlExpressionNode funcNode = parseExpression(fd);

                expr = new ErlDynamicFunctionNode(null, moduleNode, funcNode, null);

                break;
            }

            case "tuple": {
                parseInteger(); // ignore line number
                accept(',');

                final ErlExpressionNode elementNodes[] = parseExpressionArray(fd, false);
                expr = new ErlTupleNode(null, elementNodes);
                break;
            }

            case "match": {
                parseInteger(); // ignore line number
                accept(',');

                boundVariables.pushAcceptAll();
                final ErlExpressionNode leftNode = parseExpression(fd);
                boundVariables.popAcceptAll();
                accept(',');
                final ErlExpressionNode rightNode = parseExpression(fd);

                expr = new ErlMatchNode(null, leftNode, rightNode);
                break;
            }

            case "if": {
                parseInteger(); // ignore line number
                accept(',');

                final int arity = 0;
                int refArity[] = new int[]{arity};
                boundVariables.pushAcceptAll();
                expr = new ErlIfNode(null, parseClauseList(fd, refArity, false));
                boundVariables.popAcceptAll();

                break;
            }

            case "case": {
                parseInteger(); // ignore line number
                accept(',');

                final ErlExpressionNode valueNode = parseExpression(fd);
                accept(',');

                final int arity = 1; // each clause in "case" have exactly 1 match argument
                int refArity[] = new int[]{arity};
                boundVariables.pushAcceptAll();
                expr = new ErlCaseNode(null, valueNode, parseClauseList(fd, refArity, false));
                boundVariables.popAcceptAll();

                break;
            }

            case "try": {
                parseInteger(); // ignore line number
                accept(',');

                final int arity = 1; // each clause in "case" have exactly 1 match argument
                int refArity[] = new int[]{arity};

                final ErlExpressionNode valueNode = parseExpressionBlock(fd, false);
                accept(',');
                boundVariables.pushAcceptAll();
                final ErlClauseSelectorNode selectorNode = parseClauseList(fd, refArity, true);
                accept(',');
                final ErlClauseSelectorNode exceptionNode = parseClauseList(fd, refArity, true);
                accept(',');
                final ErlExpressionNode afterNode = parseExpressionBlock(fd, false);
                boundVariables.popAcceptAll();

                expr = new ErlTryNode(null, valueNode, selectorNode, exceptionNode, afterNode);

                break;
            }

            case "catch": {
                parseInteger(); // ignore line number
                accept(',');

                final ErlExpressionNode valueNode = parseExpression(fd);

                expr = new ErlCatchNode(null, valueNode);
                break;
            }

            case "block": {
                parseInteger(); // ignore line number
                accept(',');
                expr = parseExpressionBlock(fd, true);
                break;
            }

            case "lc": {
                parseInteger(); // ignore line number
                accept(',');

                boundVariables.pushAcceptAll();
                boundVariables.push(); // **** PUSH 1 ****
                boundVariables.push(); // **** PUSH 2 ****
                boundVariables.beginAcceptAll();
                final ErlExpressionNode exprNode = parseExpression(fd);
                boundVariables.endAcceptAll();
                boundVariables.pop(); // **** POP 2 ****

                accept(',');
                final ErlExpressionNode[] qualNodes = parseExpressionArray(fd, true);
                for (int i = 0; i < qualNodes.length; ++i) {
                    if (!(qualNodes[i] instanceof ErlListGeneratorNode) && !(qualNodes[i] instanceof ErlBinGeneratorNode)) {
                        qualNodes[i] = ErlToBooleanNodeGen.create(qualNodes[i].getSourceSection(), qualNodes[i]);
                    }
                }

                boundVariables.pop(); // **** POP 1 ****
                boundVariables.popAcceptAll();

                expr = new ErlListComprehensionNode(null, exprNode, qualNodes);
                break;
            }

            case "generate": {
                parseInteger(); // ignore line number
                accept(',');

                final ErlExpressionNode matchNode = parseExpression(fd);
                accept(',');
                final ErlExpressionNode listNode = parseExpression(fd);

                expr = new ErlListGeneratorNode(null, matchNode, listNode);
                break;
            }

            case "bc": {
                parseInteger(); // ignore line number
                accept(',');

                boundVariables.pushAcceptAll();
                boundVariables.push(); // **** PUSH 1 ****
                boundVariables.push(); // **** PUSH 2 ****
                boundVariables.beginAcceptAll();
                final ErlExpressionNode exprNode = parseExpression(fd);
                final Set<String> changes1 = boundVariables.getChangeSet();
                boundVariables.endAcceptAll();
                boundVariables.pop(); // **** POP 2 ****

                accept(',');
                final ErlExpressionNode[] qualNodes = parseExpressionArray(fd, true);
                for (int i = 0; i < qualNodes.length; ++i) {
                    if (!(qualNodes[i] instanceof ErlListGeneratorNode) && !(qualNodes[i] instanceof ErlBinGeneratorNode)) {
                        qualNodes[i] = ErlToBooleanNodeGen.create(qualNodes[i].getSourceSection(), qualNodes[i]);
                    }
                }

                final Set<String> changes2 = boundVariables.getChangeSet();
                assert changes1.equals(changes2);
                boundVariables.pop(); // **** POP 1 ****
                boundVariables.popAcceptAll();

                expr = new ErlBinComprehensionNode(null, exprNode, qualNodes);
                break;
            }

            case "b_generate": {
                parseInteger(); // ignore line number
                accept(',');

                final ErlExpressionNode matchNode = parseExpression(fd);
                accept(',');
                final ErlExpressionNode listNode = parseExpression(fd);

                expr = new ErlBinGeneratorNode(null, matchNode, listNode);
                break;
            }

            case "map_field_assoc": {
                parseInteger(); // ignore line number
                accept(',');

                final ErlExpressionNode keyNode = parseExpression(fd);
                accept(',');
                final ErlExpressionNode valueNode = parseExpression(fd);

                expr = new ErlMapFieldAssocNode(null, keyNode, valueNode);
                break;
            }

            case "map_field_exact": {
                parseInteger(); // ignore line number
                accept(',');

                final ErlExpressionNode keyNode = parseExpression(fd);
                accept(',');
                final ErlExpressionNode valueNode = parseExpression(fd);

                expr = new ErlMapFieldExactNode(null, keyNode, valueNode);
                break;
            }

            case "map": {
                parseInteger(); // ignore line number
                accept(',');

                ErlExpressionNode optionalMapNode = null;

                if (!nextIs('[')) {
                    optionalMapNode = parseExpression(fd);
                    accept(',');
                }

                final ErlExpressionNode[] valueNodes = parseExpressionArray(fd, false);

                expr = new ErlMapNode(null, optionalMapNode, valueNodes);
                break;
            }

            case "bin": {
                parseInteger(); // ignore line number
                accept(',');

                final List<ErlExpressionNode> valueNodes = parseExpressions(fd, false);

                expr = new ErlBinNode(null, valueNodes.toArray(new ErlBinElementNode[valueNodes.size()]));
                break;
            }

            case "bin_element": {
                parseInteger(); // ignore line number
                accept(',');

                final ErlExpressionNode valueNode = parseExpression(fd);
                accept(',');
                ErlExpressionNode sizeNode = null;
                if (nextIs('{')) {
                    sizeNode = parseExpression(fd);
                } else {
                    acceptAtom("default");
                }
                accept(',');
                ErlBinElementNode.TypeSpecifier[] spec = null;
                if (nextIs('[')) {
                    spec = parseBinTypeSpecifiers();
                } else {
                    acceptAtom("default");
                }

                if (valueNode instanceof ErlListConsNode) {
                    accept('}');
                    throw new PackedBinElementException((ErlListConsNode) valueNode, spec);
                }
                if (valueNode instanceof ErlListNilNode) {
                    accept('}');
                    throw new PackedBinElementException((ErlListNilNode) valueNode, spec);
                }

                expr = new ErlBinElementNode(null, valueNode, sizeNode, spec);
                break;
            }

            case "receive": {
                parseInteger(); // ignore line number
                accept(',');

                final int[] refArity = new int[]{1};
                final ErlClauseSelectorNode clauseSelector = parseClauseList(fd, refArity, true);
                ErlExpressionNode timeoutNode = null;
                ErlExpressionNode afterNode = null;

                if (weakAccept(',')) {
                    timeoutNode = parseExpression(fd);
                    accept(',');
                    afterNode = parseExpressionBlock(fd, true);
                }

                expr = new ErlReceiveNode(null, clauseSelector, timeoutNode, afterNode);
                break;
            }

            default:
                throw new RuntimeException("The expression kind is not implemented yet: " + kind);
        }

        accept('}');

        assert null != expr;
        return expr;
    }

    // TODO: remove this function
    @Deprecated
    private static ErlExpressionNode decomposeStringToListCons(String str) {
        ErlExpressionNode cons = new ErlListNilNode(null);

        ArrayList<Long> list = new ArrayList<>();

        for (int i = 0, n = str.length(); i < n; ++i) {

            if ('\\' == str.charAt(i)) {
                if (i + 1 < n) {
                    ++i;
                    switch (str.charAt(i)) {

                        case '\\':
                            list.add((long) '\\');
                            break;

                        case '\"':
                            list.add((long) '\"');
                            break;

                        case 'r':
                            list.add((long) '\r');
                            break;

                        case 'n':
                            list.add((long) '\n');
                            break;

                        case 't':
                            list.add((long) '\t');
                            break;

                        case 'f':
                            list.add((long) '\f');
                            break;

                        case 'e':
                            list.add(27l);
                            break;

                        default:
                            throw new RuntimeException("Unknown escape sequence");
                    }
                }
            } else {
                list.add((long) str.charAt(i));
            }
        }

        for (int i = list.size() - 1; i >= 0; --i) {
            cons = new ErlListConsNode(null, new ErlLongLiteralNode(null, list.get(i)), cons);
        }

        return cons;
    }

    private ErlExpressionNode parseFun(FrameDescriptor fd) {

        parseInteger(); // ignore line number
        accept(',');
        accept('{');
        final String funType = parseAtom();

        switch (funType) {
            case "function": {
                return parseFunAsFunction(fd);
            }

            case "clauses": {
                return parseClausesAsFunction(fd);
            }

            default:
                throw new RuntimeException("Unknown 'fun' type: " + funType);
        }
    }

    private ErlExpressionNode parseNamedFun(FrameDescriptor fd0) {

        parseInteger(); // ignore line number
        accept(',');
        final String funName = parseAtom();
        accept(',');

        boundVariables.push();
        boundVariables.add(funName);

        StackableSet.CollectAccessOf<String> accessOf = new StackableSet.CollectAccessOf<>(boundVariables.get());
        boundVariables.monitorAdd(accessOf);

        int arity = ErlFunction.UNRESOLVED_ARITY;
        int refArity[] = new int[]{arity};

        FrameDescriptor fd = fd0.shallowCopy();
        FrameSlot slot = fd.findOrAddFrameSlot(funName);
        final ErlLocalVariableNode variableNode = ErlLocalVariableNodeGen.create(null, slot);

        final ErlClauseSelectorNode selector = parseClauseList(fd, refArity, false);

        // correct the arity
        arity = refArity[0];

        final String name = "-fun/" + funName + "-" + generateUniqueId() + "-" + arity + "-";
        boundVariables.monitorRemove(accessOf);
        accessOf.access.remove(funName);

        final ErlFunctionLiteralNode funclitNode = new ErlFunctionLiteralNode(null, moduleName, name, arity);
        final ErlExpressionNode funclitReplacementNode = createCaptureNode(funclitNode, accessOf.access, fd0);
        final ErlExpressionNode bindFunNode = new ErlMatchNode(null, variableNode, funclitReplacementNode);
        final ErlExpressionNode preludeNode = createPreludeNode(accessOf.access, fd, bindFunNode);
        ErlFunctionBodyNode bodyNode = new ErlFunctionBodyNode(null, preludeNode, selector);
        bodyNode.markAsTail();
        ErlRootNode rootNode = new ErlRootNode(fd, bodyNode, new MFA(moduleName, name, arity));

        boundVariables.pop();

        ErlFunction func = module.register(name, arity, rootNode, ErlFunction.Origin.ANONYMOUS);
        assert func.getModule().equals(moduleName);
        assert func.getName().equals(name);
        assert func.getArity() == arity;

        return funclitReplacementNode;
    }

    private ErlExpressionNode parseFunAsFunction(FrameDescriptor fd) {

        ErlExpressionNode expr = null;

        accept(',');

        if (nextIs('{')) {

            final ErlExpressionNode moduleNode = parseExpression(fd);
            accept(',');
            final ErlExpressionNode funcNode = parseExpression(fd);
            accept(',');
            final ErlExpressionNode arityNode = parseExpression(fd);

            // do a little work during parse to spare time at runtime
            if ((moduleNode instanceof ErlAtomLiteralNode) && (funcNode instanceof ErlAtomLiteralNode) && (arityNode instanceof ErlLongLiteralNode)) {

                final String extModuleName = ((ErlAtomLiteralNode) moduleNode).getValue().getValue();
                final String extFuncName = ((ErlAtomLiteralNode) funcNode).getValue().getValue();
                final long extArity = ((ErlLongLiteralNode) arityNode).getValue();

                if (0 <= extArity && extArity <= Integer.MAX_VALUE) {

                    expr = new ErlFunctionLiteralNode(null, extModuleName, extFuncName, (int) extArity);

                } else {

                    // we cannot create a function literal node, because the arity is out of
                    // range, and we need the runtime error in this case
                    expr = new ErlDynamicFunctionNode(null, moduleNode, funcNode, arityNode);
                }

            } else {

                expr = new ErlDynamicFunctionNode(null, moduleNode, funcNode, arityNode);
            }

        } else {

            final String funcName = parseAtom();
            accept(',');
            final int arity = (int) parseInteger();

            expr = new ErlFunctionLiteralNode(null, moduleName, funcName, arity);
        }

        accept('}');

        assert null != expr;
        return expr;
    }

    private ErlBinElementNode.TypeSpecifier parseBinTypeSpecifier() {

        if (weakAccept('{')) {

            acceptAtom("unit");
            accept(',');
            final long num = parseInteger();
            if (num < Integer.MIN_VALUE || num > Integer.MAX_VALUE) {
                throw new RuntimeException("Invalid binary unit size specifier: " + num);
            }
            accept('}');
            return ErlBinElementNode.TypeSpecifier.unit((int) num);

        } else {

            final String atom = parseAtom();

            switch (atom) {
                case "integer":
                    return ErlBinElementNode.TypeSpecifier.INTEGER;

                case "float":
                    return ErlBinElementNode.TypeSpecifier.FLOAT;

                case "binary":
                case "bytes":
                    return ErlBinElementNode.TypeSpecifier.BINARY;

                case "bitstring":
                case "bits":
                    return ErlBinElementNode.TypeSpecifier.BITSTRING;

                case "little":
                    return ErlBinElementNode.TypeSpecifier.LITTLE;

                case "big":
                    return ErlBinElementNode.TypeSpecifier.BIG;

                case "native":
                    return ErlBinElementNode.TypeSpecifier.NATIVE;

                case "signed":
                    return ErlBinElementNode.TypeSpecifier.SIGNED;

                case "unsigned":
                    return ErlBinElementNode.TypeSpecifier.UNSIGNED;

                case "utf8":
                    return ErlBinElementNode.TypeSpecifier.UTF8;

                case "utf16":
                    return ErlBinElementNode.TypeSpecifier.UTF16;

                case "utf32":
                    return ErlBinElementNode.TypeSpecifier.UTF32;

                default:
                    throw new RuntimeException("Invalid binary element type specifier: " + atom);
            }
        }
    }

    private ErlBinElementNode.TypeSpecifier[] parseBinTypeSpecifiers() {

        ArrayList<ErlBinElementNode.TypeSpecifier> spec = new ArrayList<>();

        accept('[');

        do {

            spec.add(parseBinTypeSpecifier());

        } while (weakAccept(','));

        accept(']');

        return spec.toArray(new ErlBinElementNode.TypeSpecifier[spec.size()]);
    }

    private static ErlExpressionNode genUnaryOperator(final String op, ErlExpressionNode lhs) {
        switch (op) {
            case "+":
                return lhs;

            case "-":
                return ErlUnaryNegateNodeGen.create(null, lhs);

            case "not":
                return ErlLogicalNotNodeGen.create(null, toBoolean(lhs));

            case "bnot":
                return ErlUnaryBitwiseNotNodeGen.create(null, lhs);
        }

        throw new RuntimeException("No such unary operator: " + op);
    }

    private static ErlExpressionNode genBinaryOperator(final String op, ErlExpressionNode lhs, ErlExpressionNode rhs) {
        switch (op) {
            case "+":
                return ErlAddNodeGen.create(null, lhs, rhs);

            case "-":
                return ErlSubtractNodeGen.create(null, lhs, rhs);

            case "*":
                return ErlMultiplyNodeGen.create(null, lhs, rhs);

            case "/":
                return ErlFloatDivideNodeGen.create(null, lhs, rhs);

            case "div":
                return ErlIntegerDivideNodeGen.create(null, lhs, rhs);

            case "rem":
                return ErlIntegerRemainderNodeGen.create(null, lhs, rhs);

            case "and":
            case "andalso":
                return ErlLogicalAndNodeGen.create(null, toBoolean(lhs), toBoolean(rhs));

            case "or":
            case "orelse":
                return ErlLogicalOrNodeGen.create(null, toBoolean(lhs), toBoolean(rhs));

            case "xor":
                return ErlLogicalXorNodeGen.create(null, toBoolean(lhs), toBoolean(rhs));

            case "<":
                return ErlLessThanNodeGen.create(null, lhs, rhs);

            case ">=":
                return ErlLogicalNotNodeGen.create(null, ErlLessThanNodeGen.create(null, lhs, rhs));

            case ">":
                // note: left-hand side and right-hand sides are swapped
                return ErlLessThanNodeGen.create(null, rhs, lhs);

            case "=<":
                // note: left-hand side and right-hand sides are swapped
                return ErlLogicalNotNodeGen.create(null, ErlLessThanNodeGen.create(null, rhs, lhs));

            case "==":
                return ErlEqualNodeGen.create(null, lhs, rhs);

            case "/=":
                return ErlLogicalNotNodeGen.create(null, ErlEqualNodeGen.create(null, lhs, rhs));

            case "=:=":
                return ErlExactEqualNodeGen.create(null, lhs, rhs);

            case "=/=":
                return ErlLogicalNotNodeGen.create(null, ErlExactEqualNodeGen.create(null, lhs, rhs));

            case "band":
                return ErlBitwiseAndNodeGen.create(null, lhs, rhs);

            case "bor":
                return ErlBitwiseOrNodeGen.create(null, lhs, rhs);

            case "bxor":
                return ErlBitwiseXorNodeGen.create(null, lhs, rhs);

            case "bsl":
                return ErlBitshiftLeftNodeGen.create(null, lhs, rhs);

            case "bsr":
                return ErlBitshiftRightNodeGen.create(null, lhs, rhs);

            case "++":
                return new ErlListAddNode(null, lhs, rhs);

            case "--":
                return ErlListSubtractNodeGen.create(null, lhs, rhs);

            case "!":
                return ErlSendNodeGen.create(null, lhs, rhs);
        }

        throw new RuntimeException("No such binary operator: " + op);
    }
}