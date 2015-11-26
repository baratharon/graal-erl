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
import com.oracle.truffle.erl.runtime.ErlPid;
import com.oracle.truffle.erl.runtime.ErlProcess;

/**
 * Creates a link between the calling process and another process (or port) PidOrPort, if there is
 * not such a link already. If a process attempts to create a link to itself, nothing is done.
 * Returns true.
 * <p>
 * If PidOrPort does not exist, the behavior of the BIF depends on if the calling process is
 * trapping exits or not (see process_flag/2):
 * <ul>
 * <li>If the calling process is not trapping exits, and checking PidOrPort is cheap -- that is, if
 * PidOrPort is local -- link/1 fails with reason noproc.
 * <li>Otherwise, if the calling process is trapping exits, and/or PidOrPort is remote, link/1
 * returns true, but an exit signal with reason noproc is sent to the calling process.
 * </ul>
 */
@NodeInfo(shortName = "link")
public abstract class LinkBuiltin extends ErlBuiltinNode {

    public LinkBuiltin() {
        super(SourceSection.createUnavailable("Erlang builtin", "link"));
    }

    @Override
    public MFA getName() {
        return new MFA("erlang", "link", 1);
    }

    @Specialization
    public boolean link(ErlPid pid) {
        ErlProcess.getCurrentProcess().link(pid);
        return true;
    }

    @Specialization
    public boolean link(Object pidOrPort) {

        if (pidOrPort instanceof ErlPid) {
            return link((ErlPid) pidOrPort);
        }

        throw ErlControlException.makeBadarg();
    }
}
