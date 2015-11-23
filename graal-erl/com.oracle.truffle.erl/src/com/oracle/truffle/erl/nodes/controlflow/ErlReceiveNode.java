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

import java.util.concurrent.TimeUnit;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.nodes.ErlExpressionNode;
import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlProcess;

/**
 * The node that implements the <code>receive</code> construction in Erlang.
 * <p>
 * Receives messages sent to the process using the send operator (<code>!</code>). The patterns
 * Pattern are sequentially matched against the first message in time order in the mailbox, then the
 * second, and so on. If a match succeeds and the optional guard sequence <code>GuardSeq</code> is
 * true, the corresponding <code>Body</code> is evaluated. The matching message is consumed, that
 * is, removed from the mailbox, while any other messages in the mailbox remain unchanged.
 * <p>
 * The return value of <code>Body</code> is the return value of the receive expression.
 * <p>
 * <code>receive</code> never fails. The execution is suspended, possibly indefinitely, until a
 * message arrives that matches one of the patterns and with a true guard sequence.
 * <p>
 * <code>ExprT</code> is to evaluate to an integer. The highest allowed value is
 * <code>16#FFFFFFFF</code>, that is, the value must fit in 32 bits. <code>receive..after</code>
 * works exactly as receive, except that if no matching message has arrived within
 * <code>ExprT</code> milliseconds, then <code>BodyT</code> is evaluated instead. The return value
 * of <code>BodyT</code> then becomes the return value of the <code>receive..after</code>
 * expression.
 */
@NodeInfo(shortName = "receive", description = "")
public final class ErlReceiveNode extends ErlExpressionNode {

    @Child private ErlClauseSelectorNode clauseSelector;
    @Child private ErlExpressionNode timeoutNode;
    @Child private ErlExpressionNode afterNode;

    public ErlReceiveNode(SourceSection src, ErlClauseSelectorNode clauseSelector, ErlExpressionNode timeoutNode, ErlExpressionNode afterNode) {
        super(src);
        this.clauseSelector = clauseSelector;
        this.timeoutNode = timeoutNode;
        this.afterNode = afterNode;

        assert null != clauseSelector || null != afterNode;
        assert (null == timeoutNode && null == afterNode) || (null != timeoutNode && null != afterNode);
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {

        final long timeout = evalTimeout(frame);

        ErlProcess proc = ErlProcess.getCurrentProcess();

        // First we need to check whether an already received message fulfills the selector

        if (null != clauseSelector) {
            for (Object msg : proc.receivedMessages()) {

                proc.removeSpecificMessage(msg);

                try {
                    return clauseSelector.doSelect(frame, new Object[]{msg});
                } catch (ErlNoClauseMatchedException ex) {
                    proc.ungetMessage(msg);

                    // ignore, and loop will continue
                }
            }
        }

        // After all pending messages are checked (if there was a clause selector), and no message
        // matched, check for zero timeout. If the timeout value is not infinity, then there must be
        // an afterNode.

        if (0 == timeout) {
            assert null != afterNode;
            return afterNode.executeGeneric(frame);
        }

        final long endTime = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(timeout);

        while (timeout < 0 || System.nanoTime() < endTime) {

            Object receivedTerm;

            if (timeout < 0) {
                // System.err.println("" + ErlProcess.getSelfPid() + " #### wait infinity");
                receivedTerm = proc.receiveMessage();
            } else {
                // System.err.println("" + ErlProcess.getSelfPid() + " #### wait time " + timeout);
                receivedTerm = proc.receiveMessage(TimeUnit.NANOSECONDS.toMillis(endTime - System.nanoTime()));
            }

            if (null == receivedTerm) {
                // System.err.println("" + ErlProcess.getSelfPid() + " #### no receive after " +
                // timeout);
                continue;
            }

            proc.removeSpecificMessage(receivedTerm);

            try {

                if (null != clauseSelector) {
                    return clauseSelector.doSelect(frame, new Object[]{receivedTerm});
                } else if (null != afterNode) {
                    return afterNode.executeGeneric(frame);
                }

            } catch (ErlNoClauseMatchedException ex) {

                proc.ungetMessage(receivedTerm);
                // continue the loop
            }
        }

        // If there is no afterNode, then the timeout shall be infinity, so the control flow won't
        // reach this.

        assert null != afterNode;
        return afterNode.executeGeneric(frame);
    }

    private long evalTimeout(VirtualFrame frame) {

        if (null != timeoutNode) {

            Object result = timeoutNode.executeGeneric(frame);

            if (ErlAtom.INFINITY.equals(result)) {
                return -1;
            }

            final long timeout = ErlContext.decodeLong(result);

            if (0 <= timeout && timeout <= Integer.MAX_VALUE) {
                return timeout;
            }

            throw ErlControlException.makeBadarg();
        }

        return -1;
    }

    @Override
    public void markAsTail() {
        if (null != clauseSelector) {
            clauseSelector.markAsTail();
        }

        if (null != afterNode) {
            afterNode.markAsTail();
        }
    }
}