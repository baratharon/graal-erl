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

import java.util.zip.Adler32;

/**
 * Adler32 helper.
 */
public final class Adler {

    // largest prime smaller than 65536
    static final private int BASE = 65521;

    public Adler() {
    }

    public final static class Addler32Visitor extends IODataVisitor {

        private Adler32 adler = new Adler32();
        private long size = 0;

        @Override
        protected void visit(byte b) {
            ++size;
            adler.update(b);
        }

        @Override
        protected void visit(final byte[] bs) {
            size += bs.length;
            adler.update(bs);
        }

        public long getValue() {
            return adler.getValue();
        }

        public long getSize() {
            return size;
        }
    }

    // The following logic has come from zlib.1.2.
    public static long combine(long adler1, long adler2, long len2) {
        long BASEL = BASE;
        long sum1;
        long sum2;
        long rem;  // unsigned int

        rem = len2 % BASEL;
        sum1 = adler1 & 0xffffL;
        sum2 = rem * sum1;
        sum2 %= BASEL; // MOD(sum2);
        sum1 += (adler2 & 0xffffL) + BASEL - 1;
        sum2 += ((adler1 >> 16) & 0xffffL) + ((adler2 >> 16) & 0xffffL) + BASEL - rem;

        if (sum1 >= BASEL)
            sum1 -= BASEL;
        if (sum1 >= BASEL)
            sum1 -= BASEL;
        if (sum2 >= (BASEL << 1))
            sum2 -= (BASEL << 1);
        if (sum2 >= BASEL)
            sum2 -= BASEL;

        return sum1 | (sum2 << 16);
    }
}
