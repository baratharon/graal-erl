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

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.nodes.ErlExpressionNode;
import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.ErlTuple;

/**
 * The node that implements the "try" construction in Erlang.
 * <p>
 * It returns the value of Exprs (a sequence of expressions Expr1, ..., ExprN) unless an exception
 * occurs during the evaluation. In that case the exception is caught and the patterns
 * ExceptionPattern with the right exception class Class are sequentially matched against the caught
 * exception. An omitted Class is shorthand for throw. If a match succeeds and the optional guard
 * sequence ExceptionGuardSeq is true, the corresponding ExceptionBody is evaluated to become the
 * return value.
 * <p>
 * If an exception occurs during evaluation of Exprs but there is no matching ExceptionPattern of
 * the right Class with a true guard sequence, the exception is passed on as if Exprs had not been
 * enclosed in a try expression.
 * <p>
 * If an exception occurs during evaluation of ExceptionBody, it is not caught.
 * <p>
 * If the evaluation of Exprs succeeds without an exception, the patterns Pattern are sequentially
 * matched against the result in the same way as for a case expression, except that if the matching
 * fails, a try_clause run-time error occurs.
 * <p>
 * An exception occurring during the evaluation of Body is not caught.
 * <p>
 * AfterBody is evaluated after either Body or ExceptionBody, no matter which one. The evaluated
 * value of AfterBody is lost; the return value of the try expression is the same with an after
 * section as without.
 * <p>
 * Even if an exception occurs during evaluation of Body or ExceptionBody, AfterBody is evaluated.
 * In this case the exception is passed on after AfterBody has been evaluated, so the exception from
 * the try expression is the same with an after section as without.
 * <p>
 * If an exception occurs during evaluation of AfterBody itself, it is not caught. So if AfterBody
 * is evaluated after an exception in Exprs, Body, or ExceptionBody, that exception is lost and
 * masked by the exception in AfterBody.
 */
@NodeInfo(shortName = "try", description = "")
public final class ErlTryNode extends ErlExpressionNode {

    @Child private ErlExpressionNode valueNode;
    @Child private ErlClauseSelectorNode clauseSelector;
    @Child private ErlClauseSelectorNode exceptionSelector;
    @Child private ErlExpressionNode afterNode;

    public ErlTryNode(SourceSection src, ErlExpressionNode valueNode, ErlClauseSelectorNode clauseSelector, ErlClauseSelectorNode exceptionSelector, ErlExpressionNode afterNode) {
        super(src);
        this.valueNode = valueNode;
        this.clauseSelector = clauseSelector;
        this.exceptionSelector = exceptionSelector;
        this.afterNode = afterNode;

        assert null != valueNode;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {

        Object value;

        try {
            try {

                value = valueNode.executeGeneric(frame);

                if (null != clauseSelector) {
                    value = clauseSelector.doSelect(frame, new Object[]{value});

                    if (null == value) {
                        throw ErlControlException.makeTryClause(value);
                    }
                }

            } catch (ErlControlException ex) {

                ErlTuple exceptionData = ex.getDescribingTerm();

                assert (1 < exceptionData.getSize());

                if (ErlAtom._EXIT == exceptionData.getElement(1)) {

                    if (ErlControlException.SpecialTag.EXIT == ex.getSpecialTag()) {

                        exceptionData = new ErlTuple(ErlAtom.EXIT, exceptionData.getElement(2), ErlList.NIL);

                    } else if (exceptionData.getElement(2) instanceof ErlTuple) {

                        ErlTuple second = (ErlTuple) exceptionData.getElement(2);

                        exceptionData = new ErlTuple(ErlAtom.ERROR, second.getElement(1), second.getElement(2));
                    }
                }

                final Object exResult = exceptionSelector.doSelect(frame, new Object[]{exceptionData});

                if (null != exResult) {
                    return exResult;
                }

                throw ex;
            }
        } finally {
            if (null != afterNode) {
                afterNode.executeGeneric(frame);
            }
        }

        return value;
    }

    @Override
    public void markAsTail() {

        if (null == afterNode) {

            if (null != clauseSelector) {
                clauseSelector.markAsTail();
            }

            if (null != exceptionSelector) {
                exceptionSelector.markAsTail();
            }
        }
    }
}
