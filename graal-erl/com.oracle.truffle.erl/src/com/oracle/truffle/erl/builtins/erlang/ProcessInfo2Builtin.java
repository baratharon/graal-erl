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

import java.util.ArrayList;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.builtins.ErlBuiltinNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.MFA;
import com.oracle.truffle.erl.runtime.ErlPid;
import com.oracle.truffle.erl.runtime.ErlProcess;
import com.oracle.truffle.erl.runtime.ErlTuple;

/**
 * Returns information about the process identified by Pid as specified by the Item or the ItemList,
 * or undefined if the process is not alive.
 * <p>
 * If the process is alive and a single Item is given, the returned value is the corresponding
 * InfoTuple unless Item =:= registered_name and the process has no registered name. In this case []
 * is returned. This strange behavior is due to historical reasons, and is kept for backward
 * compatibility.
 */
@NodeInfo(shortName = "processInfo")
public abstract class ProcessInfo2Builtin extends ErlBuiltinNode {

    public ProcessInfo2Builtin() {
        super(SourceSection.createUnavailable("Erlang builtin", "process_info"));
    }

    @Override
    public MFA[] getNames() {
        return new MFA[]{new MFA("erlang", "process_info", 2)};
    }

    private static ErlTuple getInfo(ErlProcess proc, ErlAtom item) {

        Object value;

        if (ErlAtom.REGISTERED_NAME.equals(item)) {
            String name = proc.getRegisteredName();
            if (null != name) {
                value = new ErlAtom(name);
            } else {
                value = ErlList.NIL;
            }

        } else if (ErlAtom.DICTIONARY.equals(item)) {

            ErlContext.PairListBuilderBiConsumer builder = new ErlContext.PairListBuilderBiConsumer();
            ErlProcess.dictForEach(builder);
            value = builder.getResult();

        } else if (ErlAtom.MESSAGES.equals(item)) {

            ErlContext.ListBuilderConsumer builder = new ErlContext.ListBuilderConsumer();
            ErlProcess.forEachMessages(builder);
            value = builder.getResult();

        } else if (ErlAtom.LINKS.equals(item)) {

            ErlContext.ListBuilderConsumer builder = new ErlContext.ListBuilderConsumer();
            ErlProcess.forEachLink(builder);
            value = builder.getResult();

        } else if (ErlAtom.STATUS.equals(item)) {

            if (ErlProcess.getCurrentProcess() == proc) {
                value = ErlAtom.RUNNING;
            } else {
                value = ErlAtom.RUNNABLE;
            }

        } else if (ErlAtom.TRAP_EXIT.equals(item)) {

            value = ErlProcess.getTrapExit();

        } else if (ErlAtom.GROUP_LEADER.equals(item)) {

            value = ErlProcess.getCurrentProcess().getGroupLeader();

        } else if (ErlAtom.HEAP_SIZE.equals(item)) {

            value = (long) 50000;

        } else if (ErlAtom.STACK_SIZE.equals(item)) {

            value = (long) 42;

        } else if (ErlAtom.REDUCTIONS.equals(item)) {

            value = (long) 0;

        } else {
            ErlContext.notImplemented();
            throw ErlControlException.makeBadarg();
        }

        return new ErlTuple(item, value);
    }

    @Specialization
    public Object processInfo(ErlPid pid, ErlAtom item) {

        ErlProcess proc = ErlProcess.findProcess(pid);

        if (null == proc) {
            return ErlAtom.UNDEFINED;
        }

        if (ErlAtom.REGISTERED_NAME.equals(item)) {
            if (null == proc.getRegisteredName()) {
                return ErlList.NIL;
            }
        }

        return getInfo(proc, item);
    }

    @Specialization
    public Object processInfo(ErlPid pid, ErlList itemlist_) {

        ArrayList<Object> result = new ArrayList<>();
        ErlList itemlist = itemlist_;
        ErlProcess proc = ErlProcess.findProcess(pid);

        if (null == proc) {
            return ErlAtom.UNDEFINED;
        }

        while (ErlList.NIL != itemlist) {

            result.add(getInfo(proc, ErlAtom.fromObject(itemlist.getHead())));

            itemlist = itemlist.getTailList();
        }

        return ErlList.fromList(result);
    }

    @Specialization
    public Object processInfo(Object arg1, Object arg2) {

        if (arg2 instanceof ErlList) {
            return processInfo(ErlPid.fromObject(arg1), ErlList.fromObject(arg2));
        }

        return processInfo(ErlPid.fromObject(arg1), ErlAtom.fromObject(arg2));
    }
}
