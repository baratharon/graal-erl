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
package com.oracle.truffle.erl.builtins.erlang;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.MFA;
import com.oracle.truffle.erl.builtins.ErlBuiltinNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlBinary;
import com.oracle.truffle.erl.runtime.ErlPort;
import com.oracle.truffle.erl.runtime.ErlProcess;
import com.oracle.truffle.erl.runtime.misc.IoList;

/**
 * Sends data to a port. Same as Port ! {PortOwner, {command, Data}} except for the error behaviour
 * and being synchronous (see below). Any process may send data to a port with port_command/2, not
 * only the port owner (the connected process).
 * <p>
 * For comparison: <code>Port ! {PortOwner, {command, Data}}</code> fails with badarg if Port cannot
 * be sent to (i.e., Port refers neither to a port nor to a process). If Port is a closed port the
 * data message disappears without a sound. If Port is open and the calling process is not the port
 * owner, the <b>port owner</b> fails with badsig. The port owner fails with badsig also if Data is
 * not a valid IO list.
 * <p>
 * Note that any process can send to a port using <code>Port ! {PortOwner, {command, Data}}</code>
 * just as if it itself was the port owner.
 * <p>
 * If the port is busy, the calling process will be suspended until the port is not busy anymore.
 * <p>
 * As of OTP-R16 <code>Port ! {PortOwner, {command, Data}}</code> is truly asynchronous. Note that
 * this operation has always been documented as an asynchronous operation, while the underlying
 * implementation has been synchronous. port_command/2 is however still fully synchronous. This due
 * to its error behavior.
 */
@NodeInfo(shortName = "portCommand")
public abstract class PortCommand2Builtin extends ErlBuiltinNode {

    public PortCommand2Builtin() {
        super(SourceSection.createUnavailable("Erlang builtin", "port_command"));
    }

    @Override
    public MFA[] getNames() {
        return new MFA[]{new MFA("erlang", "port_command", 2)};
    }

    @Specialization
    public boolean portCommand(ErlPort port, Object data) {

        ErlBinary bin = IoList.toBinary(data);

        if (port.isOpen()) {
            port.command(ErlProcess.getSelfPid(), bin.toByteArray(), false);
            return true;
        }

        throw ErlControlException.makeBadarg();
    }

    @Specialization
    public boolean portCommand(ErlAtom arg1, Object data) {
        return portCommand(ErlProcess.findRegistered(ErlPort.class, arg1.getValue()), data);
    }

    @Specialization
    public boolean portCommand(Object arg1, Object arg2) {

        if (arg1 instanceof ErlPort) {
            return portCommand((ErlPort) arg1, arg2);
        }

        return portCommand(ErlAtom.fromObject(arg1), arg2);
    }
}
