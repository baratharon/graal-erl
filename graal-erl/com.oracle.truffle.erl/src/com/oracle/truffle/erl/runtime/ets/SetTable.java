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
package com.oracle.truffle.erl.runtime.ets;

import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;
import java.util.function.BiConsumer;

import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.ErlTuple;

final class SetTable extends ErlTable {

    private static final ArrayList<ErlTuple> EMPTY = new ArrayList<>();
    private final TreeMap<Object, ErlTuple> elements;

    protected SetTable(Object tableID, ErlAtom name, TableOptions options) {
        super(tableID, name, options);
        elements = new TreeMap<>(ErlContext.TERM_COMPARATOR);
    }

    @Override
    public TableType getTableType() {
        return TableType.SET;
    }

    @Override
    protected boolean insertTuple(ErlTuple tuple) {
        return null != elements.put(tuple.getElement(keypos), tuple);
    }

    @Override
    protected boolean removeTuple(ErlTuple tuple) {
        return null == elements.remove(tuple.getElement(keypos));
    }

    @Override
    protected List<ErlTuple> lookupTuples(Object key) {

        final ErlTuple tuple = elements.get(key);

        if (null != tuple) {
            final ArrayList<ErlTuple> result = new ArrayList<>();
            result.add(tuple);
            return result;
        }

        return EMPTY;
    }

    @Override
    protected void selectByPattern(final List<ErlTuple> out, final MatchPattern matchPattern) {

        elements.forEach(new BiConsumer<Object, ErlTuple>() {

            public void accept(Object key, ErlTuple tuple) {
                MatchContext ctx = new MatchContext();
                if (matchPattern.match(ctx, tuple)) {
                    out.add(tuple);
                }
            }
        });
    }

    @Override
    protected void selectByMatch(final List<ErlList> out, final MatchPattern matchPattern) {

        elements.forEach(new BiConsumer<Object, ErlTuple>() {

            public void accept(Object key, ErlTuple tuple) {
                MatchContext ctx = new MatchContext();
                if (matchPattern.match(ctx, tuple)) {
                    out.add(ctx.toErlList());
                }
            }
        });
    }

    @Override
    protected ErlTuple getTupleByIndex(int index_) {

        try {
            final int[] index = new int[]{index_};

            elements.forEach(new BiConsumer<Object, ErlTuple>() {

                public void accept(Object key, ErlTuple tuple) {
                    if (0 == (index[0]--)) {
                        throw new TupleFound(tuple);
                    }
                }
            });

            return null;
        } catch (TupleFound ex) {
            return ex.tuple;
        }
    }
}
