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

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.MFA;
import com.oracle.truffle.erl.builtins.ErlBuiltinNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.ErlTuple;

/**
 * Returns various information about the allocators of the current system (emulator) as specified by
 * Item:
 */
@NodeInfo(shortName = "systemInfo")
public abstract class SystemInfoBuiltin extends ErlBuiltinNode {

    public SystemInfoBuiltin() {
        super(SourceSection.createUnavailable("Erlang builtin", "system_info"));
    }

    @Override
    public MFA getName() {
        return new MFA("erlang", "system_info", 1);
    }

    @Specialization
    @TruffleBoundary
    public Object systemInfo(ErlAtom item) {

        if (ErlAtom.THREADS.equals(item)) {
            return true;
        }

        if (ErlAtom.THREAD_POOL_SIZE.equals(item)) {
            return (long) Runtime.getRuntime().availableProcessors();
        }

        if (ErlAtom.OS_TYPE.equals(item)) {

            if (ErlContext.isWindows()) {
                return new ErlTuple(ErlAtom.WIN32, ErlAtom.NT);
            } else if (ErlContext.isUnix()) {
                return new ErlTuple(ErlAtom.UNIX, ErlAtom.LINUX);
            } else {
                return new ErlTuple(ErlAtom.MAC, (long) 1);
            }
        }

        if (ErlAtom.HIPE_ARCHITECTURE.equals(item)) {
            return new ErlAtom(ErlContext.OS_ARCH);
        }

        if (ErlAtom.VERSION.equals(item)) {
            return new ErlList((long) '7', new ErlList((long) '.', new ErlList((long) '1', ErlList.NIL)));
        }

        ErlContext.notImplemented();
        throw ErlControlException.makeBadarg();
    }

    @Specialization
    public Object systemInfo(ErlTuple item) {

        if (2 == item.getSize()) {

            if (ErlAtom.PURIFY.equals(item.getElement(1))) {
                return true;
            }

        }

        ErlContext.notImplemented();
        throw ErlControlException.makeBadarg();
    }

    @Specialization
    public Object systemInfo(Object item) {

        if (item instanceof ErlTuple) {
            return systemInfo((ErlTuple) item);
        }

        return systemInfo(ErlAtom.fromObject(item));
    }
}
