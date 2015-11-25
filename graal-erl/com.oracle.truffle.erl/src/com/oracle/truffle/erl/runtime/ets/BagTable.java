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

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.TreeSet;

import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.ErlTuple;

final class BagTable extends ErlTable {

    private final TreeSet<ErlTuple> elements;

    protected BagTable(Object tableID, ErlAtom name, TableOptions options) {
        super(tableID, name, options);
        elements = new TreeSet<>(ErlContext.TERM_COMPARATOR_EXACT);
    }

    @Override
    public TableType getTableType() {
        return TableType.BAG;
    }

    @Override
    protected boolean insertTuple(ErlTuple tuple) {
        return elements.add(tuple);
    }

    @Override
    protected boolean removeTuple(ErlTuple tuple) {
        return elements.remove(tuple);
    }

    @Override
    protected List<ErlTuple> lookupTuples(Object key) {

        final LinkedList<ErlTuple> result = new LinkedList<>();

        for (Iterator<ErlTuple> iter = elements.iterator(); iter.hasNext();) {
            final ErlTuple tuple = iter.next();

            if (0 == ErlContext.compareTerms(key, tuple.getElement(keypos), true)) {
                result.addFirst(tuple);
            }
        }

        return result;
    }

    @Override
    protected void selectByPattern(final List<ErlTuple> out, final MatchPattern matchPattern) {

        for (Iterator<ErlTuple> iter = elements.iterator(); iter.hasNext();) {
            final ErlTuple tuple = iter.next();

            MatchContext ctx = new MatchContext();
            if (matchPattern.match(ctx, tuple)) {
                out.add(tuple);
            }
        }
    }

    @Override
    protected void selectByMatch(final List<ErlList> out, final MatchPattern matchPattern) {

        for (Iterator<ErlTuple> iter = elements.iterator(); iter.hasNext();) {
            final ErlTuple tuple = iter.next();

            MatchContext ctx = new MatchContext();
            if (matchPattern.match(ctx, tuple)) {
                out.add(ctx.toErlList());
            }
        }
    }

    @Override
    protected ErlTuple getTupleByIndex(int index_) {

        int index = index_;

        for (Iterator<ErlTuple> iter = elements.iterator(); iter.hasNext();) {
            final ErlTuple tuple = iter.next();

            if (0 == (index--)) {
                return tuple;
            }
        }

        return null;
    }
}
