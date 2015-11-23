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

/**
 * The node that implements the "case" construction in Erlang.
 * <p>
 * The expression Expr is evaluated and the patterns Pattern are sequentially matched against the
 * result. If a match succeeds and the optional guard sequence GuardSeq is true, the corresponding
 * Body is evaluated.
 * <p>
 * The return value of Body is the return value of the case expression.
 * <p>
 * If there is no matching pattern with a true guard sequence, a case_clause run-time error occurs.
 */
@NodeInfo(shortName = "if", description = "")
public final class ErlCaseNode extends ErlExpressionNode {

    @Child private ErlExpressionNode valueNode;
    @Child private ErlClauseSelectorNode clauseSelector;

    public ErlCaseNode(SourceSection src, ErlExpressionNode valueNode, ErlClauseSelectorNode clauseSelector) {
        super(src);
        this.valueNode = valueNode;
        this.clauseSelector = clauseSelector;

        assert null != valueNode;
        assert null != clauseSelector;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {

        final Object value = valueNode.executeGeneric(frame);

        try {

            return clauseSelector.doSelect(frame, new Object[]{value});

        } catch (ErlNoClauseMatchedException ex) {

            throw ErlControlException.makeCaseClause(value);
        }
    }

    @Override
    public void markAsTail() {
        clauseSelector.markAsTail();
    }
}
