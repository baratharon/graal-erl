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
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlFunction;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.MFA;
import com.oracle.truffle.erl.runtime.ErlPid;
import com.oracle.truffle.erl.runtime.ErlProcess;

/**
 * Returns the pid of a new process started by the application of Module:Function to Args. The new
 * process created will be placed in the system scheduler queue and be run some time later.
 * <p>
 * error_handler:undefined_function(Module, Function, Args) is evaluated by the new process if
 * Module:Function/Arity does not exist (where Arity is the length of Args). The error handler can
 * be redefined (see process_flag/2). If error_handler is undefined, or the user has redefined the
 * default error_handler its replacement is undefined, a failure with the reason undef will occur.
 */
@NodeInfo(shortName = "spawn")
public abstract class Spawn3Builtin extends ErlBuiltinNode {

    public Spawn3Builtin() {
        super(SourceSection.createUnavailable("Erlang builtin", "spawn"));
    }

    @Override
    public MFA[] getNames() {
        return new MFA[]{new MFA("erlang", "spawn", 3)};
    }

    @Specialization
    public ErlPid spawn(ErlAtom module, ErlAtom func, ErlList arglist) {

        Object[] args = arglist.toArray();

        ErlContext context = ErlProcess.getContext();
        ErlFunction fun = context.getFunctionRegistry().lookup(module.getValue(), func.getValue(), args.length);

        return ErlProcess.spawn(context, null, null, fun, module.getValue(), func.getValue(), args).getPid();
    }

    @Specialization
    public ErlPid spawn(Object module, Object func, Object arglist) {
        return spawn(ErlAtom.fromObject(module), ErlAtom.fromObject(func), ErlList.fromObject(arglist));
    }
}
