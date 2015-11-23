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

import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.ErlPid;
import com.oracle.truffle.erl.runtime.ErlTuple;

public final class TableOptions {

    // fields are package private for quick access
    TableType type;
    TableAccess access;
    boolean namedTable;
    ErlPid heir;
    Object heirData;
    int keypos;
    boolean writeConcurrency;
    boolean readConcurrency;

    private TableOptions() {
        this.type = null;
        this.access = null;
        this.namedTable = false;
        this.heir = null;
        this.heirData = null;
        this.keypos = 1;
        this.writeConcurrency = false;
        this.readConcurrency = false;
    }

    public TableOptions(TableType type, TableAccess access, boolean namedTable, ErlPid heir, Object heirData, int keypos, boolean writeConcurrency, boolean readConcurrency) {
        this.type = type;
        this.access = access;
        this.namedTable = namedTable;
        this.heir = heir;
        this.heirData = heirData;
        this.keypos = keypos;
        this.writeConcurrency = writeConcurrency;
        this.readConcurrency = readConcurrency;
    }

    public static TableOptions parse(ErlList opts_) {

        TableOptions result = new TableOptions();

        main: for (ErlList opts = opts_; ErlList.NIL != opts; opts = opts.getTailList()) {

            Object hd = opts.getHead();

            for (TableType tt : TableType.values()) {
                if (tt.atom.equals(hd)) {
                    if (null == result.type) {
                        result.type = tt;
                    }
                    continue main;
                }
            }

            for (TableAccess ta : TableAccess.values()) {
                if (ta.atom.equals(hd)) {
                    if (null == result.access) {
                        result.access = ta;
                    }
                    continue main;
                }
            }

            if (ErlAtom.NAMED_TABLE.equals(hd)) {
                result.namedTable = true;
            } else if (ErlAtom.COMPRESSED.equals(hd)) {
                // ignored
            } else if (hd instanceof ErlTuple) {

                final ErlTuple tup = (ErlTuple) hd;

                if (2 == tup.getSize() && ErlAtom.KEYPOS.equals(tup.getElement(1))) {
                    result.keypos = ErlContext.decodeInt(tup.getElement(2));
                } else if (2 == tup.getSize() && ErlAtom.HEIR.equals(tup.getElement(1)) && ErlAtom.NONE.equals(tup.getElement(2))) {
                    result.heir = null;
                    result.heirData = null;
                } else if (3 == tup.getSize() && ErlAtom.HEIR.equals(tup.getElement(1)) && (tup.getElement(2) instanceof ErlPid)) {
                    result.heir = (ErlPid) tup.getElement(2);
                    result.heirData = tup.getElement(3);
                } else if (2 == tup.getSize() && ErlAtom.WRITE_CONCURRENCY.equals(tup.getElement(1))) {
                    final Object second = tup.getElement(2);
                    if (ErlAtom.TRUE.equals(second)) {
                        result.writeConcurrency = true;
                    } else if (ErlAtom.FALSE.equals(second)) {
                        result.writeConcurrency = false;
                    } else {
                        throw ErlControlException.makeBadarg();
                    }
                } else if (2 == tup.getSize() && ErlAtom.READ_CONCURRENCY.equals(tup.getElement(1))) {
                    final Object second = tup.getElement(2);
                    if (ErlAtom.TRUE.equals(second)) {
                        result.readConcurrency = true;
                    } else if (ErlAtom.FALSE.equals(second)) {
                        result.readConcurrency = false;
                    } else {
                        throw ErlControlException.makeBadarg();
                    }
                } else {
                    throw ErlControlException.makeBadarg();
                }

            } else {
                throw ErlControlException.makeBadarg();
            }
        }

        if (null == result.type) {
            result.type = TableType.SET;
        }

        if (null == result.access) {
            result.access = TableAccess.PROTECTED;
        }

        return result;
    }

    public boolean isNamedTable() {
        return namedTable;
    }
}
