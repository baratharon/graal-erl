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

import com.oracle.truffle.api.dsl.ShortCircuit;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.nodes.ErlBinaryNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlPid;
import com.oracle.truffle.erl.runtime.ErlPort;
import com.oracle.truffle.erl.runtime.ErlProcess;
import com.oracle.truffle.erl.runtime.ErlTuple;
import com.oracle.truffle.erl.runtime.misc.IoList;

/**
 * Sends the value of Expr2 as a message to the process specified by Expr1. The value of Expr2 is
 * also the return value of the expression.
 * <p>
 * Expr1 must evaluate to a pid, a registered name (atom), or a tuple {Name,Node}. Name is an atom
 * and Node is a node name, also an atom.
 * <ul>
 * <li>If Expr1 evaluates to a name, but this name is not registered, a badarg run-time error
 * occurs.</li>
 * <li>Sending a message to a pid never fails, even if the pid identifies a non-existing process.
 * </li>
 * <li>Distributed message sending, that is, if Expr1 evaluates to a tuple {Name,Node} (or a pid
 * located at another node), also never fails.</li>
 * </ul>
 */
@NodeInfo(shortName = "!")
@SuppressWarnings("unused")
public abstract class ErlSendNode extends ErlBinaryNode {

    public ErlSendNode(SourceSection src) {
        super(src);
    }

    @Specialization
    protected Object send(ErlPid pid, Object msg) {
        ErlProcess.send(pid, msg, false, false);
        return msg;
    }

    @Specialization
    protected Object send(ErlPort port, ErlTuple tuple) {

        if (2 == tuple.getSize()) {

            final ErlPid sender = ErlPid.fromObject(tuple.getElement(1));
            final Object second = tuple.getElement(2);

            if (ErlAtom.CLOSE.equals(second)) {
                port.closeAsync(sender);
                return tuple;
            } else if (second instanceof ErlTuple) {

                final ErlTuple inner = (ErlTuple) second;

                if (2 == inner.getSize()) {

                    final ErlAtom type = ErlAtom.fromObject(inner.getElement(1));

                    if (ErlAtom.COMMAND.equals(type)) {
                        port.command(sender, IoList.toArray(inner.getElement(2)), false);
                        return tuple;
                    } else if (ErlAtom.CONNECT.equals(type)) {
                        ErlContext.notImplemented();
                    }
                }
            }
        }

        throw ErlControlException.makeBadarg();
    }

    @Specialization
    protected Object send(ErlPort port, Object msg) {
        return send(port, ErlTuple.fromObject(msg));
    }

    @Specialization
    protected Object send(ErlAtom name, Object msg) {
        ErlProcess.send(name, msg, false, false);
        return msg;
    }

    @Specialization
    protected Object send(Object arg1, Object arg2) {

        if (arg1 instanceof ErlPid) {
            return send((ErlPid) arg1, arg2);
        }

        if (arg1 instanceof ErlPort) {
            return send((ErlPort) arg1, arg2);
        }

        return send(ErlAtom.fromObject(arg1), arg2);
    }
}
