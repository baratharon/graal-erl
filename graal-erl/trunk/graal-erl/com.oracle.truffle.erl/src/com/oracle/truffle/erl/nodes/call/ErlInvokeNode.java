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
package com.oracle.truffle.erl.nodes.call;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.NodeChildren;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.ForeignAccess;
import com.oracle.truffle.api.interop.Message;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.nodes.ErlExpressionNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.nodes.controlflow.ErlTailCallException;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlFunction;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.ErlTuple;

/**
 * The node for function invocation in Erlang. Since Erlang has first class functions, the
 * {@link ErlFunction target function} can be computed by an arbitrary expression. This node is
 * responsible for evaluating this expression, as well as evaluating the {@link #argumentNodes
 * arguments}. The actual dispatch is then delegated to a chain of {@link ErlDispatchNode} that form
 * a polymorphic inline cache.
 */
@NodeInfo(shortName = "invoke")
@NodeChildren({@NodeChild(value = "functionNode", type = ErlExpressionNode.class)})
public abstract class ErlInvokeNode extends ErlExpressionNode {
    @Children private final ErlExpressionNode[] argumentNodes;
    @Child private ErlDispatchNode dispatchNode;
    @CompilationFinal private boolean tailCall = false;

    ErlInvokeNode(SourceSection src, ErlExpressionNode[] argumentNodes) {
        super(src);
        this.argumentNodes = argumentNodes;
        this.dispatchNode = ErlDispatchNodeGen.create();
    }

    @Override
    public void markAsTail() {
        CompilerDirectives.transferToInterpreterAndInvalidate();
        this.tailCall = true;
    }

    @Specialization
    public Object executeGeneric(VirtualFrame frame, ErlFunction function) {

        Object[] argumentValues = evaluateArgumentNodes(frame);

        /*
         * To report the 'badarity' error, we must provide all actual arguments. That's why we
         * evaluated the arguments before checking the arity.
         */
        if (argumentNodes.length != function.getArity()) {
            throw ErlControlException.makeBadarity(new ErlTuple(function, ErlList.fromArray(argumentValues)));
        }

        if (tailCall) {
            throw new ErlTailCallException(function, argumentValues);
        }

        ErlFunction func = function;
        Object[] args = argumentValues;

        for (;;) {

            try {
                return dispatchNode.executeDispatch(frame, func, args);
            } catch (ErlTailCallException tailCallEx) {

                func = tailCallEx.getFunction();
                args = tailCallEx.getArguments();
            }
        }
    }

    @ExplodeLoop
    private Object[] evaluateArgumentNodes(VirtualFrame frame) {
        /*
         * The number of arguments is constant for one invoke node. During compilation, the loop is
         * unrolled and the execute methods of all arguments are inlined. This is triggered by the
         * ExplodeLoop annotation on the method. The compiler assertion below illustrates that the
         * array length is really constant.
         */
        CompilerAsserts.compilationConstant(argumentNodes.length);

        Object[] argumentValues = new Object[argumentNodes.length];
        for (int i = 0; i < argumentNodes.length; i++) {
            argumentValues[i] = argumentNodes[i].executeGeneric(frame);
        }

        return argumentValues;
    }

    @Child private Node crossLanguageCall;

    @Specialization
    @ExplodeLoop
    protected Object executeGeneric(VirtualFrame frame, TruffleObject function) {
        /*
         * The number of arguments is constant for one invoke node. During compilation, the loop is
         * unrolled and the execute methods of all arguments are inlined. This is triggered by the
         * ExplodeLoop annotation on the method. The compiler assertion below illustrates that the
         * array length is really constant.
         */
        CompilerAsserts.compilationConstant(argumentNodes.length);

        Object[] argumentValues = new Object[argumentNodes.length];
        for (int i = 0; i < argumentNodes.length; i++) {
            argumentValues[i] = argumentNodes[i].executeGeneric(frame);
        }
        if (crossLanguageCall == null) {
            crossLanguageCall = insert(Message.createExecute(argumentValues.length).createNode());
        }
        Object res = ForeignAccess.execute(crossLanguageCall, frame, function, argumentValues);
        return ErlContext.fromForeignValue(res);
    }
}
