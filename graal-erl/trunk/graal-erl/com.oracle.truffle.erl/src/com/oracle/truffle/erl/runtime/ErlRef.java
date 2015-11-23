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
package com.oracle.truffle.erl.runtime;

import java.util.Comparator;

import com.oracle.truffle.api.interop.ForeignAccess;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;

/**
 * The Erlang references are represented as a 128-bit integer.
 */
public final class ErlRef implements TruffleObject {

    private final long high;
    private final long low;

    private static final long genHigh = 0;
    private static long genLow = 1;

    public synchronized static ErlRef make() {
        return new ErlRef(genHigh, genLow++);
    }

    public static final Comparator<ErlRef> COMPARATOR = new Comparator<ErlRef>() {

        public int compare(ErlRef lhs, ErlRef rhs) {
            return lhs.compare(rhs);
        }
    };

    private ErlRef(long high, long low) {
        this.high = high;
        this.low = low;
    }

    public static ErlRef fromObject(Object obj) {

        if (obj instanceof ErlRef) {
            return (ErlRef) obj;
        }

        throw ErlControlException.makeBadarg();
    }

    public long getHighPart() {
        return high;
    }

    public long getLowPart() {
        return low;
    }

    @Override
    public String toString() {
        return "#Ref<" + (high >>> 32) + "." + (high & 0xFFFFFFFF) + "." + (low >>> 32) + "." + (low & 0xFFFFFFFF) + ">";
    }

    public int compare(ErlRef rhs) {

        final int result = Long.compare(high, rhs.high);
        if (0 != result) {
            return result;
        }

        return Long.compare(low, rhs.low);
    }

    @Override
    public ForeignAccess getForeignAccess() {
        return ErlFunctionForeignAccess.create();
    }
}
