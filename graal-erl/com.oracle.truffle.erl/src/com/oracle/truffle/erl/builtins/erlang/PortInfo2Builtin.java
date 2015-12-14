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
import com.oracle.truffle.erl.runtime.ErlPort;
import com.oracle.truffle.erl.runtime.ErlProcess;
import com.oracle.truffle.erl.runtime.misc.PortInfoItem;

/**
 * Pid is the process identifier of the process connected to the port.
 * <p>
 * If the port identified by Port is not open, undefined is returned. If undefined is returned and
 * the calling process was linked to a previously open port identified by Port, an exit signal due
 * to this link was received by the process prior to the return from port_info/2.
 * <p>
 * Failure: badarg if Port is not a local port identifier, or an atom.
 */
@NodeInfo(shortName = "portInfo")
public abstract class PortInfo2Builtin extends ErlBuiltinNode {

    public PortInfo2Builtin() {
        super(SourceSection.createUnavailable("Erlang builtin", "port_info"));
    }

    @Override
    public MFA getName() {
        return new MFA("erlang", "port_info", 2);
    }

    public static Object getPortInfo(ErlPort port, ErlAtom item) {
        if (port.isOpen()) {

            for (PortInfoItem infoItem : PortInfoItem.values()) {
                if (infoItem.atom.equals(item)) {
                    return port.getInfo(infoItem);
                }
            }

            if (ErlAtom.REGISTERED_NAME.equals(item)) {
                return port.getRegisteredNameAtom();
            }

            throw ErlControlException.makeBadarg();

        } else {
            return ErlAtom.UNDEFINED;
        }
    }

    @Specialization
    public Object portInfo(ErlPort port, ErlAtom item) {
        return getPortInfo(port, item);
    }

    @Specialization
    public Object portInfo(ErlAtom arg1, ErlAtom item) {
        final ErlPort port = ErlProcess.findRegistered(ErlPort.class, arg1.getValue());
        if (null != port) {
            return portInfo(port, item);
        }
        throw ErlControlException.makeBadarg();
    }

    @Specialization
    public Object portInfo(Object arg1, Object arg2) {

        if (arg1 instanceof ErlAtom) {
            return portInfo((ErlAtom) arg1, ErlAtom.fromObject(arg2));
        }

        return portInfo(ErlPort.fromObject(arg1), ErlAtom.fromObject(arg2));
    }
}
