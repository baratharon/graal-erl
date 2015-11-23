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
 * The node that implements the "if" construction in Erlang.
 * <p>
 * The branches of an if-expression are scanned sequentially until a guard sequence GuardSeq that
 * evaluates to true is found. Then the corresponding Body (sequence of expressions separated by
 * ',') is evaluated.
 * <p>
 * The return value of Body is the return value of the if expression.
 * <p>
 * If no guard sequence is evaluated as true, an if_clause run-time error occurs. If necessary, the
 * guard expression true can be used in the last branch, as that guard sequence is always true.
 */
@NodeInfo(shortName = "if", description = "")
public final class ErlIfNode extends ErlExpressionNode {

    @Child private ErlClauseSelectorNode clauseSelector;

    private static final Object EMPTY_ARRAY[] = new Object[0];

    public ErlIfNode(SourceSection src, ErlClauseSelectorNode clauseSelector) {
        super(src);
        this.clauseSelector = clauseSelector;

        assert null != clauseSelector;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {

        try {

            // since the 'if' statement does not have any "arguments", passing an empty array of
            // Object is good enough

            return clauseSelector.doSelect(frame, EMPTY_ARRAY);

        } catch (ErlNoClauseMatchedException ex) {

            throw ErlControlException.makeIfClause();
        }
    }

    @Override
    public void markAsTail() {
        clauseSelector.markAsTail();
    }
}
