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
import com.oracle.truffle.erl.builtins.ErlBuiltinNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.MFA;
import com.oracle.truffle.erl.runtime.misc.IoList;
import com.oracle.truffle.erl.runtime.ErlPort;
import com.oracle.truffle.erl.runtime.ErlProcess;

/**
 * Performs a synchronous control operation on a port. The meaning of Operation and Data depends on
 * the port, i.e., on the port driver. Not all port drivers support this control feature.
 * <p>
 * Returns: a list of integers in the range 0 through 255, or a binary, depending on the port
 * driver. The meaning of the returned data also depends on the port driver.
 * <p>
 * Failure: badarg if Port is not an open port or the registered name of an open port, if Operation
 * cannot fit in a 32-bit integer, if the port driver does not support synchronous control
 * operations, or if the port driver so decides for any reason (probably something wrong with
 * Operation or Data).
 */
@NodeInfo(shortName = "portControl")
public abstract class PortControlBuiltin extends ErlBuiltinNode {

    public PortControlBuiltin() {
        super(SourceSection.createUnavailable("Erlang builtin", "port_control"));
    }

    @Override
    public MFA[] getNames() {
        return new MFA[]{new MFA("erlang", "port_control", 3)};
    }

    @Specialization
    public Object portControl(ErlPort port, long operation, Object data) {

        if (Integer.MIN_VALUE <= operation && operation <= Integer.MAX_VALUE) {

            return port.control((int) operation, IoList.toArray(data));
        }

        throw ErlControlException.makeBadarg();
    }

    @Specialization
    public Object portControl(ErlAtom arg1, long operation, Object data) {
        return portControl(ErlProcess.findRegistered(ErlPort.class, arg1.getValue()), operation, data);
    }

    @Specialization
    public Object portControl(Object arg1, Object arg2, Object arg3) {

        if (arg1 instanceof ErlPort) {
            return portControl((ErlPort) arg1, ErlContext.decodeLong(arg2), arg3);
        }

        return portControl(ErlAtom.fromObject(arg1), ErlContext.decodeLong(arg2), arg3);
    }
}
