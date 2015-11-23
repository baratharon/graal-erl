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

import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.ErlPid;
import com.oracle.truffle.erl.runtime.ErlProcess;
import com.oracle.truffle.erl.runtime.ErlTuple;

public abstract class ErlTable {

    private static final TreeMap<Object, ErlTable> TABLES = new TreeMap<>(ErlContext.TERM_COMPARATOR);

    private final Object tableID;
    private ErlPid owner;
    private ErlPid heir;
    private Object heirData;
    private boolean namedTable;
    private ErlAtom name;
    protected final int keypos;
    private TableAccess protection;
    private boolean writeConcurrency;
    private boolean readConcurrency;

    protected ErlTable(Object tableID, ErlAtom name, TableOptions options) {
        this.tableID = tableID;
        this.owner = ErlProcess.getSelfPid();
        this.heir = options.heir;
        this.heirData = options.heirData;
        this.namedTable = options.namedTable;
        this.name = name;
        this.keypos = options.keypos;
        this.protection = options.access;
        this.writeConcurrency = options.writeConcurrency;
        this.readConcurrency = options.readConcurrency;

        ErlProcess.getCurrentProcess().addTable(this);
        TABLES.put(tableID, this);
    }

    protected static class TupleFound extends RuntimeException {

        private static final long serialVersionUID = -588256975215353709L;

        final ErlTuple tuple;

        public TupleFound(ErlTuple tuple) {
            this.tuple = tuple;
        }
    }

    private static long nextTableID = 1;

    private static synchronized Long generateNewTableID() {
        return (ErlProcess.getSelfPid().getId() << 17) | (nextTableID++);
    }

    public static ErlTable createTable(ErlAtom name, TableOptions options) {

        Object tableID;

        if (options.namedTable) {
            tableID = name;
        } else {
            tableID = generateNewTableID();
        }

        switch (options.type) {
            case SET:
                return new SetTable(tableID, name, options);

            case BAG:
                return new BagTable(tableID, name, options);

            case DUPLICATE_BAG:
                ErlContext.notImplemented();

            case ORDERED_SET:
                ErlContext.notImplemented();

            default:
                throw ErlControlException.makeBadarg();
        }
    }

    public static ErlTable findTable(Object tid) {
        synchronized (TABLES) {
            return TABLES.get(tid);
        }
    }

    public void forget() {
        synchronized (TABLES) {
            TABLES.remove(tableID);
        }
    }

    public synchronized boolean insert(ErlTuple tuple) {
        checkWriteAccess();
        return insertTuple(tuple);
    }

    public synchronized boolean insert(ErlList list) {
        checkWriteAccess();

        for (ErlList l = list; ErlList.NIL != l; l = l.getTailList()) {
            insertTuple(ErlTuple.fromObject(l.getHead()));
        }

        return true;
    }

    public synchronized List<ErlTuple> lookup(Object key) {
        checkReadAccess();
        return lookupTuples(key);
    }

    public synchronized ErlTuple slot(long index) {
        checkReadAccess();
        if (0 <= index && index <= Integer.MAX_VALUE) {
            return getTupleByIndex((int) index);
        }
        return null;
    }

    public synchronized void deleteWithKey(Object key) {

        checkWriteAccess();

        for (ErlTuple tuple : lookupTuples(key)) {
            removeTuple(tuple);
        }
    }

    public synchronized long selectDelete(ErlList matchSpec) {

        checkWriteAccess();

        ArrayList<ErlTuple> out = new ArrayList<>();

        for (ErlList list = matchSpec; ErlList.NIL != list; list = list.getTailList()) {
            final ErlTuple spec = ErlTuple.fromObject(list.getHead());

            if (3 == spec.getSize()) {
                final MatchPattern pattern = MatchPattern.compile(spec.getElement(1));
                selectByPattern(out, pattern);
            } else {
                throw ErlControlException.makeBadarg();
            }
        }

        long deleted = 0;

        for (ErlTuple tup : out) {
            if (removeTuple(tup)) {
                ++deleted;
            }
        }

        return deleted;
    }

    public synchronized ErlList match(Object matchPattern) {

        checkWriteAccess();

        ArrayList<ErlList> out = new ArrayList<>();

        final MatchPattern pattern = MatchPattern.compile(matchPattern);
        selectByMatch(out, pattern);

        return ErlList.fromIterator(out.iterator());
    }

    public void onProcessDied() {
        if (null != heir) {
            final ErlTuple msg = new ErlTuple(ErlAtom._ETS_TRANSFER, tableID, owner, heirData);
            if (ErlAtom.OK == ErlProcess.send(heir, msg, false, false)) {
                return;
            }
        }

        TABLES.remove(tableID);
    }

    public Object getTableID() {
        return tableID;
    }

    private void checkWriteAccess() {
        if (TableAccess.PUBLIC != protection && !ErlProcess.getSelfPid().equals(owner)) {
            throw ErlControlException.makeBadarg();
        }
    }

    private void checkReadAccess() {
        if (TableAccess.PRIVATE == protection && !ErlProcess.getSelfPid().equals(owner)) {
            throw ErlControlException.makeBadarg();
        }
    }

    /**
     * @return true if the tuple was really deleted
     */
    protected abstract boolean removeTuple(ErlTuple tuple);

    protected abstract List<ErlTuple> lookupTuples(Object key);

    protected abstract ErlTuple getTupleByIndex(int index);

    protected abstract void selectByPattern(final List<ErlTuple> out, final MatchPattern matchPattern);

    protected abstract void selectByMatch(final List<ErlList> out, final MatchPattern matchPattern);

    public abstract TableType getTableType();

    protected abstract boolean insertTuple(ErlTuple tuple);
}
