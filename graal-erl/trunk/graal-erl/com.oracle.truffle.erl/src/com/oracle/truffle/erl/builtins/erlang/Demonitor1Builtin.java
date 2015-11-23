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
import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.MFA;
import com.oracle.truffle.erl.runtime.ErlProcess;
import com.oracle.truffle.erl.runtime.ErlRef;

/**
 * If MonitorRef is a reference which the calling process obtained by calling monitor/2, this
 * monitoring is turned off. If the monitoring is already turned off, nothing happens.
 * <p>
 * Once demonitor(MonitorRef) has returned it is guaranteed that no {'DOWN', MonitorRef, _, _, _}
 * message due to the monitor will be placed in the caller's message queue in the future. A {'DOWN',
 * MonitorRef, _, _, _} message might have been placed in the caller's message queue prior to the
 * call, though. Therefore, in most cases, it is advisable to remove such a 'DOWN' message from the
 * message queue after monitoring has been stopped. demonitor(MonitorRef, [flush]) can be used
 * instead of demonitor(MonitorRef) if this cleanup is wanted.
 */
@NodeInfo(shortName = "demonitor")
public abstract class Demonitor1Builtin extends ErlBuiltinNode {

    public Demonitor1Builtin() {
        super(SourceSection.createUnavailable("Erlang builtin", "demonitor"));
    }

    @Override
    public MFA[] getNames() {
        return new MFA[]{new MFA("erlang", "demonitor", 1)};
    }

    @Specialization
    public boolean demonitor(ErlRef monRef) {
        ErlProcess.demonitor(monRef, false, false);
        return true;
    }

    @Specialization
    public boolean demonitor(Object arg1) {
        return demonitor(ErlAtom.fromObject(arg1));
    }
}
