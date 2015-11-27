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

import java.util.Arrays;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.nodes.ErlExpressionNode;

/**
 * Clause selector node which execute the right clause. Will throw a
 * {@link ErlNoClauseMatchedException} when no clause was matched. Note that, the
 * {@link ErlControlException}s will passed unaltered.
 */
@NodeInfo(shortName = "clauseSelector", description = "Executes the right clause.")
public final class ErlClauseSelectorNode extends ErlExpressionNode {

    /**
     * The array of child nodes. The annotation {@link com.oracle.truffle.api.nodes.Node.Children
     * Children} informs Truffle that the field contains multiple children. It is a Truffle
     * requirement that the field is {@code final} and an array of nodes.
     */
    @Children private final ErlClauseNode[] clauseNodes;

    public ErlClauseSelectorNode(SourceSection src, ErlClauseNode[] clauseNodes) {
        super(src);
        this.clauseNodes = clauseNodes;

        assert null != clauseNodes && 0 < clauseNodes.length;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {

        // in generic case, use the function arguments

        Object args[] = frame.getArguments();
        assert 0 != args.length;
        args = Arrays.copyOf(args, args.length - 1);

        return doSelect(frame, args);
    }

    @ExplodeLoop
    public Object doSelect(VirtualFrame frame, Object args[]) {

        /*
         * Check the interrupted flag here. This node is quite widely used, but still the check
         * won't be done in every single node.
         */
        if (Thread.currentThread().isInterrupted()) {
            throw ErlExitProcessException.INSTANCE;
        }

        /*
         * This assertion illustrates that the array length is really a constant during compilation.
         */
        CompilerAsserts.compilationConstant(clauseNodes.length);

        for (ErlClauseNode clause : clauseNodes) {
            try {
                return clause.doWith(frame, args);
            } catch (ErlNoClauseMatchedException ex) {
                // ignore
            }
        }

        // no clause was matched
        throw ErlNoClauseMatchedException.SINGLETON;
    }

    @Override
    public void markAsTail() {
        for (ErlClauseNode clause : clauseNodes) {
            clause.markAsTail();
        }
    }
}
