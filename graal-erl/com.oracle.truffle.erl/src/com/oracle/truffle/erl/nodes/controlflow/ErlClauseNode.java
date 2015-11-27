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
package com.oracle.truffle.erl.nodes.controlflow;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.dsl.UnsupportedSpecializationException;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.api.utilities.BinaryConditionProfile;
import com.oracle.truffle.api.utilities.ConditionProfile;
import com.oracle.truffle.api.utilities.CountingConditionProfile;
import com.oracle.truffle.erl.nodes.ErlExpressionNode;

/**
 * A statement node that executes a list of other statements if the given condition evaluated to
 * true.
 */
@NodeInfo(shortName = "clause", description = "The node implementing a function clause")
public final class ErlClauseNode extends ErlExpressionNode {

    /**
     * These nodes are used to match the arguments.
     */
    @Children private final ErlExpressionNode[] matchNodes;

    /**
     * This node is used to evaluate all the guards attached to the current clause. When the
     * condition is false, an {@link ErlNoClauseMatchedException} will be thrown.
     */
    @Child private ErlExpressionNode conditionNode;

    /**
     * The array of child nodes. The annotation {@link com.oracle.truffle.api.nodes.Node.Children
     * Children} informs Truffle that the field contains multiple children. It is a Truffle
     * requirement that the field is {@code final} and an array of nodes.
     */
    @Children private final ErlExpressionNode[] bodyNodes;

    /**
     * Profiling information, collected by the interpreter, capturing the profiling information of
     * the condition. This allows the compiler to generate better code for conditions that are
     * always true or always false. Additionally the {@link CountingConditionProfile} implementation
     * (as opposed to {@link BinaryConditionProfile} implementation) transmits the probability of
     * the condition to be true to the compiler.
     */
    private final ConditionProfile condition = ConditionProfile.createCountingProfile();

    public ErlClauseNode(SourceSection src, ErlExpressionNode[] matchNodes, ErlExpressionNode conditionNode, ErlExpressionNode... bodyNodes) {
        super(src);
        this.matchNodes = matchNodes;
        this.conditionNode = conditionNode;
        this.bodyNodes = bodyNodes;

        // we really need at least one child node
        assert null != matchNodes;
        assert null != bodyNodes && bodyNodes.length > 0;
    }

    /**
     * Execute all child statements. The annotation {@link ExplodeLoop} triggers full unrolling of
     * the loop during compilation. This allows the {@link ErlExpressionNode#executeGeneric} method
     * of all children to be inlined.
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {

        throw ErlControlException.makeUndef();
    }

    @ExplodeLoop
    public Object doWith(VirtualFrame frame, Object arguments[]) {

        CompilerAsserts.compilationConstant(matchNodes.length);

        if (matchNodes.length != arguments.length) {
            throw ErlControlException.makeFunctionClause();
        }

        for (int i = 0; i < matchNodes.length; ++i) {
            try {

                matchNodes[i].match(frame, arguments[i]);

            } catch (ErlControlException ex) {

                if (ErlControlException.SpecialTag.BADMATCH == ex.getSpecialTag()) {

                    throw ErlNoClauseMatchedException.SINGLETON;

                } else {

                    throw ex;
                }
            }
        }

        /*
         * In the interpreter, record profiling information that the condition was executed and with
         * which outcome.
         */
        if (!condition.profile(evaluateCondition(frame))) {
            throw ErlNoClauseMatchedException.SINGLETON;
        }

        /*
         * This assertion illustrates that the array length is really a constant during compilation.
         */
        CompilerAsserts.compilationConstant(bodyNodes.length);

        Object term = null; // yikes

        for (ErlExpressionNode statement : bodyNodes) {
            term = statement.executeGeneric(frame);
        }

        assert term != null; // java null cannot be returned
        return term;
    }

    private boolean evaluateCondition(VirtualFrame frame) {

        if (null == conditionNode) {
            return true;
        }

        try {
            /*
             * The condition must evaluate to a boolean value, so we call the boolean-specialized
             * execute method.
             */
            return conditionNode.executeBoolean(frame);
        } catch (ErlControlException ex) {
            return false;
        } catch (UnexpectedResultException ex) {
            /*
             * The condition evaluated to a non-boolean result. This is a type error in the Erlang
             * program. We report it with the same exception that Truffle DSL generated nodes use to
             * report type errors.
             */
            throw new UnsupportedSpecializationException(this, new Node[]{conditionNode}, ex.getResult());
        }
    }

    @Override
    public void markAsTail() {
        bodyNodes[bodyNodes.length - 1].markAsTail();
    }
}
