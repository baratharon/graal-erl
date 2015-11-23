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
package com.oracle.truffle.erl.builtins.ets;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.builtins.ErlBuiltinNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.ErlTuple;
import com.oracle.truffle.erl.runtime.MFA;
import com.oracle.truffle.erl.runtime.ets.ErlTable;

/**
 * This function is mostly for debugging purposes, Normally one should use first/next or last/prev
 * instead.
 * <p>
 * Returns all objects in the I:th slot of the table Tab. A table can be traversed by repeatedly
 * calling the function, starting with the first slot I=0 and ending when '$end_of_table' is
 * returned. The function will fail with reason badarg if the I argument is out of range.
 * <p>
 * Unless a table of type set, bag or duplicate_bag is protected using safe_fixtable/2, see above, a
 * traversal may fail if concurrent updates are made to the table. If the table is of type
 * ordered_set, the function returns a list containing the I:th object in Erlang term order.
 */
@NodeInfo(shortName = "slot")
public abstract class SlotBuiltin extends ErlBuiltinNode {

    public SlotBuiltin() {
        super(SourceSection.createUnavailable("Erlang ETS builtin", "slot"));
    }

    @Override
    public MFA[] getNames() {
        return new MFA[]{new MFA("ets", "slot", 2)};
    }

    @Specialization
    public Object lookup(Object tid, Object index) {
        final ErlTable tab = ErlTable.findTable(tid);
        if (null != tab) {

            final ErlTuple element = tab.slot(ErlContext.decodeLong(index));

            if (null != element) {
                return new ErlList(element, ErlList.NIL);
            } else {
                return ErlAtom._END_OF_TABLE;
            }
        }

        throw ErlControlException.makeBadarg();
    }
}
