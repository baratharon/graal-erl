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
package com.oracle.truffle.erl.builtins.lists;

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
 * Searches the list of tuples TupleList for a tuple whose Nth element compares equal to Key.
 * Returns Tuple if such a tuple is found, otherwise false.
 */
@NodeInfo(shortName = "keyfind")
public abstract class KeyfindBuiltin extends ErlBuiltinNode {

    public KeyfindBuiltin() {
        super(SourceSection.createUnavailable("Erlang builtin", "keyfind"));
    }

    @Override
    public MFA[] getNames() {
        return new MFA[]{new MFA("lists", "keyfind", 3)};
    }

    public static Object doKeyfind(Object key, long num, ErlList tupleList) {

        if (0 == num || num > Integer.MAX_VALUE) {
            throw ErlControlException.makeBadarg();
        }

        final int index = (int) num;

        ErlList remaining = tupleList;

        while (ErlList.NIL != remaining) {

            if (remaining.getHead() instanceof ErlTuple) {

                ErlTuple tuple = (ErlTuple) remaining.getHead();

                if (index <= tuple.getSize() && 0 == ErlContext.compareTerms(key, tuple.getElement(index), false)) {
                    return tuple;
                }
            }

            if (remaining.getTail() instanceof ErlList) {
                remaining = (ErlList) remaining.getTail();
            } else {
                throw ErlControlException.makeBadarg();
            }
        }

        return false;
    }

    @Specialization
    public Object keyfind(Object key, long num, ErlList tupleList) {
        return doKeyfind(key, num, tupleList);
    }

    @Specialization
    public Object keyfind(Object arg1, Object arg2, Object arg3) {

        return keyfind(arg1, ErlContext.decodeLong(arg2), ErlList.fromObject(arg3));
    }
}
