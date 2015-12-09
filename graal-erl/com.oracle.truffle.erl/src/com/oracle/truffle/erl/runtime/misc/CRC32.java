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

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;

/**
 * CRC-32 helper.
 * <p>
 * IEEE 802.3 style
 */
public final class CRC32 {

    public CRC32() {
    }

    public final static class CRC32Visitor extends IODataVisitor {

        private java.util.zip.CRC32 crc = new java.util.zip.CRC32();
        private long size = 0;

        @Override
        @TruffleBoundary
        protected void visit(byte b) {
            ++size;
            crc.update(b);
        }

        @Override
        @TruffleBoundary
        protected void visit(final byte[] bs) {
            size += bs.length;
            crc.update(bs);
        }

        @TruffleBoundary
        public long getValue() {
            return crc.getValue();
        }

        public long getSize() {
            return size;
        }
    }

    // The following logic has come from zlib.1.2.
    private static final int GF2_DIM = 32;

    public static long combine(long crc1_, long crc2, long len2_) {

        long crc1 = crc1_;
        long len2 = len2_;

        long row;
        long[] even = new long[GF2_DIM];
        long[] odd = new long[GF2_DIM];

        // degenerate case (also disallow negative lengths)
        if (len2 <= 0) {
            return crc1;
        }

        // put operator for one zero bit in odd
        odd[0] = 0xedb88320L;          // CRC-32 polynomial
        row = 1;
        for (int n = 1; n < GF2_DIM; n++) {
            odd[n] = row;
            row <<= 1;
        }

        // put operator for two zero bits in even
        gf2_matrix_square(even, odd);

        // put operator for four zero bits in odd
        gf2_matrix_square(odd, even);

        // apply len2 zeros to crc1 (first square will put the operator for one
        // zero byte, eight zero bits, in even)
        do {
            // apply zeros operator for this bit of len2
            gf2_matrix_square(even, odd);
            if ((len2 & 1) != 0) {
                crc1 = gf2_matrix_times(even, crc1);
            }
            len2 >>= 1;

            // if no more bits set, then done
            if (len2 == 0) {
                break;
            }

            // another iteration of the loop with odd and even swapped
            gf2_matrix_square(odd, even);
            if ((len2 & 1) != 0) {
                crc1 = gf2_matrix_times(odd, crc1);
            }
            len2 >>= 1;

            // if no more bits set, then done
        } while (len2 != 0);

        // return combined crc
        crc1 ^= crc2;
        return crc1;
    }

    private static long gf2_matrix_times(long[] mat, long vec_) {

        long vec = vec_;
        long sum = 0;
        int index = 0;
        while (vec != 0) {
            if ((vec & 1) != 0) {
                sum ^= mat[index];
            }
            vec >>= 1;
            index++;
        }
        return sum;
    }

    static final void gf2_matrix_square(long[] square, long[] mat) {

        for (int n = 0; n < GF2_DIM; n++) {
            square[n] = gf2_matrix_times(mat, mat[n]);
        }
    }
}
