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
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.ErlTuple;

/**
 * erlang:make_tuple first creates a tuple of size Arity where each element has the value
 * DefaultValue. It then fills in values from InitList. Each list element in InitList must be a
 * two-tuple where the first element is a position in the newly created tuple and the second element
 * is any term. If a position occurs more than once in the list, the term corresponding to last
 * occurrence will be used.
 */
@NodeInfo(shortName = "makeTuple")
public abstract class MakeTuple3Builtin extends ErlBuiltinNode {

    public MakeTuple3Builtin() {
        super(SourceSection.createUnavailable("Erlang builtin", "make_tuple"));
    }

    @Override
    public MFA[] getNames() {
        return new MFA[]{new MFA("erlang", "make_tuple", 3)};
    }

    @Specialization
    public ErlTuple makeTuple(long num, Object defaultElement, ErlList list) {

        if (num < 0 || num > Integer.MAX_VALUE) {
            throw ErlControlException.makeBadarg();
        }

        Object[] arr = new Object[(int) num];
        for (int i = 0; i < (int) num; ++i) {
            arr[i] = defaultElement;
        }

        ErlList tail = list;
        while (ErlList.NIL != tail) {

            Object h = tail.getHead();

            if (h instanceof ErlTuple) {

                ErlTuple init = (ErlTuple) h;

                if (2 == init.getSize()) {

                    final int idx = ErlContext.decodeInt(init.getElement(1)) - 1;

                    if (0 <= idx && idx < arr.length) {
                        arr[idx] = init.getElement(2);
                    } else {
                        throw ErlControlException.makeBadarg();
                    }
                } else {
                    throw ErlControlException.makeBadarg();
                }
            } else {
                throw ErlControlException.makeBadarg();
            }

            Object t = tail.getTail();

            if (t instanceof ErlList) {
                tail = (ErlList) t;
            } else {
                throw ErlControlException.makeBadarg();
            }
        }

        return ErlTuple.fromArray(arr);
    }

    @Specialization
    public ErlTuple makeTuple(Object arg1, Object arg2, Object arg3) {

        return makeTuple(ErlContext.decodeInt(arg1), arg2, ErlList.fromObject(arg3));
    }
}
