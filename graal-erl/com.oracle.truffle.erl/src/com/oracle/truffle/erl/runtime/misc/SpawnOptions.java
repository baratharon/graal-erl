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
package com.oracle.truffle.erl.runtime.misc;

import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.ErlProcess;
import com.oracle.truffle.erl.runtime.ErlRef;
import com.oracle.truffle.erl.runtime.ErlTuple;

public final class SpawnOptions {

    private final boolean link;
    private final boolean monitor;
    private final ErlRef[] monitorRef;

    public SpawnOptions(boolean link, boolean monitor) {
        this.link = link;
        this.monitor = monitor;

        if (monitor) {
            this.monitorRef = new ErlRef[1];
        } else {
            this.monitorRef = null;
        }
    }

    public static SpawnOptions parse(ErlList opts_) {

        boolean link = false;
        boolean monitor = false;
        ErlList opts = opts_;

        while (ErlList.NIL != opts) {

            Object hd = opts.getHead();

            if (ErlAtom.LINK.equals(hd)) {
                link = true;
            } else if (ErlAtom.MONITOR.equals(hd)) {
                monitor = true;
            } else if (hd instanceof ErlTuple) {

                ErlTuple tuple = (ErlTuple) hd;

                if (2 == tuple.getSize()) {

                    Object first = tuple.getElement(1);
                    // Object second = tuple.getElement(2);

                    if (ErlAtom.PRIORITY.equals(first)) {
                        // ignore
                    } else if (ErlAtom.FULLSWEEP_AFTER.equals(first)) {
                        // ignore
                    } else if (ErlAtom.MIN_HEAP_SIZE.equals(first)) {
                        // ignore
                    } else if (ErlAtom.MIN_BIN_VHEAP_SIZE.equals(first)) {
                        // ignore
                    } else {
                        throw ErlControlException.makeBadarg();
                    }

                } else {
                    throw ErlControlException.makeBadarg();
                }

            } else {
                throw ErlControlException.makeBadarg();
            }

            opts = opts.getTailList();
        }

        return new SpawnOptions(link, monitor);
    }

    public boolean hasLinkOption() {
        return link;
    }

    public ErlProcess link() {
        if (link) {
            return ErlProcess.getCurrentProcess();
        } else {
            return null;
        }
    }

    public boolean hasMonitorOption() {
        return monitor;
    }

    public ErlRef[] monitorRef() {
        return monitorRef;
    }
}
