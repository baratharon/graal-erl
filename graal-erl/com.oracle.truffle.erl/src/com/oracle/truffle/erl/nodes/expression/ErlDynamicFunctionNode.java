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
package com.oracle.truffle.erl.nodes.expression;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.nodes.ErlExpressionNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlFunction;
import com.oracle.truffle.erl.runtime.ErlFunctionRegistry;
import com.oracle.truffle.erl.runtime.ErlProcess;

/**
 * Dynamic resolution of a {@link ErlFunction function}. The actual function determines only at
 * runtime.
 */
@NodeInfo(shortName = "func")
public final class ErlDynamicFunctionNode extends ErlExpressionNode {
    @Child private ErlExpressionNode moduleNameNode;
    @Child private ErlExpressionNode funcNameNode;
    @Child private ErlExpressionNode arityNode;

    public ErlDynamicFunctionNode(SourceSection src, ErlExpressionNode moduleNameNode, ErlExpressionNode funcNameNode, ErlExpressionNode arityNode) {
        super(src);
        this.moduleNameNode = moduleNameNode;
        this.funcNameNode = funcNameNode;
        this.arityNode = arityNode;
    }

    @Override
    public ErlFunction executeGeneric(VirtualFrame frame) {

        if (null == arityNode) {
            // functions with unknown arity comes without arity node -- these functions definitely
            // undefined
            throw ErlControlException.makeUndef();
        }

        ErlContext context = ErlProcess.getContext();

        try {
            final String moduleName = moduleNameNode.executeAtom(frame).getValue();
            final String funcName = funcNameNode.executeAtom(frame).getValue();
            final long arity = arityNode.executeLong(frame);

            if (arity < 0 || arity > Integer.MAX_VALUE) {
                // if arity is negative or greater than max value of an integer, the function is
                // definitely undefined
                throw ErlControlException.makeUndef();
            }

            final ErlFunctionRegistry functionRegistry = context.getFunctionRegistry();
            ErlFunction function = functionRegistry.lookup(moduleName, funcName, (int) arity);

            if (null == function) {

                // if (!functionRegistry.isModuleLoaded(moduleName) &&
                // ErlContext.loadModule(moduleName)) {
                // function = functionRegistry.lookup(moduleName, funcName, (int) arity);
                // if (null != function) {
                // return function;
                // }
                // }

                throw ErlControlException.makeUndef();
            }

            return function;

        } catch (UnexpectedResultException ex) {

            throw ErlControlException.makeUndef();
        }
    }

    public ErlDynamicFunctionNode withArity(int newArity) {

        ErlLongLiteralNode newArityNode = new ErlLongLiteralNode(null, newArity);
        return new ErlDynamicFunctionNode(this.getSourceSection(), moduleNameNode, funcNameNode, newArityNode);
    }
}
