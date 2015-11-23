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

import java.math.BigInteger;
import java.util.LinkedList;

import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlBinary;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlFunction;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.ErlRef;
import com.oracle.truffle.erl.runtime.ErlTuple;

/**
 * Erlang hash functions.
 */
public final class Hash {

    public Hash() {
    }

    private static final class Tag {

        @SuppressWarnings("unused") private int x;

        public Tag(int x) {
            this.x = x;
        }
    }

    private static final int FUNNY_NUMBER1 = 268440163;
    private static final int FUNNY_NUMBER2 = 268439161;
    private static final int FUNNY_NUMBER3 = 268435459;
    private static final int FUNNY_NUMBER4 = 268436141;
    private static final int FUNNY_NUMBER5 = 268438633;
    private static final int FUNNY_NUMBER6 = 268437017;
    private static final int FUNNY_NUMBER7 = 268438039;
    private static final int FUNNY_NUMBER8 = 268437511;
    private static final int FUNNY_NUMBER9 = 268439627;
    private static final int FUNNY_NUMBER10 = 268440479;
    private static final int FUNNY_NUMBER11 = 268440577;
    private static final int FUNNY_NUMBER12 = 268440581;
    private static final int FUNNY_NUMBER13 = 268440593;
    private static final int FUNNY_NUMBER14 = 268440611;

    private static final Object MAKE_HASH_CDR_PRE_OP = new Tag(1);
    private static final Object MAKE_HASH_CDR_POST_OP = new Tag(2);
    private static final Object MAKE_HASH_TUPLE_OP = new Tag(3);

    private static long UINT32_HASH_STEP(long hash, long x, int prime) {

        return ((((hash * prime + (x & 0xFF)) * prime + ((x >>> 8) & 0xFF)) * prime + ((x >>> 16) & 0xFF)) * prime + ((x >>> 24) & 0xFF));
    }

    private static final class TailRecur extends Exception {

        private static final long serialVersionUID = -5724970133387580597L;
        public static final TailRecur INSTANCE = new TailRecur();
    }

    public static int makeHash(Object term) {

        LinkedList<Object> stack = new LinkedList<>();
        stack.push(term);

        long hash = 0;

        for (;;) {

            try {

                if (stack.isEmpty()) {
                    return (int) hash;
                }

                Object top = stack.pop();

                if (MAKE_HASH_CDR_PRE_OP == top) {
                    top = stack.pop();
                    if (ErlContext.getTermRank(top) != ErlContext.TermRank.LIST) {
                        stack.push(MAKE_HASH_CDR_POST_OP);
                    }

                } else if (MAKE_HASH_CDR_POST_OP == top) {
                    hash *= FUNNY_NUMBER8;
                    continue;
                } else if (MAKE_HASH_TUPLE_OP == top) {

                    int i = (int) stack.pop();
                    ErlTuple tuple = (ErlTuple) stack.pop();

                    if (i <= tuple.getSize()) {
                        top = tuple.getElement(i);
                        stack.push(tuple);
                        stack.push(i + 1);
                        stack.push(MAKE_HASH_TUPLE_OP);
                    } else {
                        hash = hash * FUNNY_NUMBER9 + tuple.getSize();
                        continue;
                    }
                }

                final ErlContext.TermRank rank = ErlContext.getTermRank(top);
                switch (rank) {
                    case NIL: {
                        hash = hash * FUNNY_NUMBER3 + 1;
                        break;
                    }

                    case ATOM: {
                        hash = hash * FUNNY_NUMBER1 + ((ErlAtom) top).hashCode();
                        break;
                    }

                    case NUMBER: {

                        switch (ErlContext.getNumberKind(top)) {

                            case LONG: {
                                final long y1 = (long) top;
                                final long y2 = y1 < 0 ? -y1 : y1;

                                hash = UINT32_HASH_STEP(hash, y2, FUNNY_NUMBER2);

                                final long y3 = y2 >>> 32;
                                if (0 != y3) {
                                    hash = UINT32_HASH_STEP(hash, y3, FUNNY_NUMBER2);
                                }

                                hash *= (y1 < 0 ? FUNNY_NUMBER4 : FUNNY_NUMBER3);
                                break;
                            }

                            case DOUBLE: {
                                final long L = Double.doubleToLongBits((double) top);
                                hash = hash * FUNNY_NUMBER6 + ((L >>> 32) ^ (L & 0xFFFFFFFF));
                                break;
                            }

                            case BIGINTEGER: {

                                BigInteger bi = (BigInteger) top;

                                final boolean isNeg = bi.signum() < 0;
                                final byte[] arr = bi.abs().toByteArray();

                                for (int i = arr.length - 1; i >= 0; --i) {
                                    hash = hash * FUNNY_NUMBER2 + Byte.toUnsignedLong(arr[i]);
                                }

                                // pad to 32-bit word size
                                for (int i = arr.length, n = ((arr.length + 3) & (-4)); i < n; ++i) {
                                    hash = hash * FUNNY_NUMBER2;
                                }

                                hash *= isNeg ? FUNNY_NUMBER4 : FUNNY_NUMBER3;
                                break;
                            }
                        }

                        break;
                    }

                    case LIST: {

                        ErlList list = (ErlList) top;

                        while (ErlContext.isByte(list.getHead())) {

                            hash = hash * FUNNY_NUMBER2 + ErlContext.decodeLong(list.getHead());

                            Object tl = list.getTail();

                            if (ErlList.NIL == tl || !(tl instanceof ErlList)) {
                                stack.push(MAKE_HASH_CDR_POST_OP);
                                stack.push(tl);
                                throw TailRecur.INSTANCE;
                            }

                            list = (ErlList) tl;
                        }

                        stack.push(list.getTail());
                        stack.push(MAKE_HASH_CDR_PRE_OP);
                        stack.push(list.getHead());
                        break;
                    }

                    case TUPLE: {
                        final int start_index = 1;
                        stack.push(top);
                        stack.push(start_index);
                        stack.push(MAKE_HASH_TUPLE_OP);
                        break;
                    }

                    case REFERENCE: {
                        ErlRef ref = (ErlRef) top;
                        hash = UINT32_HASH_STEP(hash, ref.getLowPart(), FUNNY_NUMBER9) * FUNNY_NUMBER10;
                        break;
                    }

                    case BIT_STRING: {

                        // The binary must be constructed, so we use the fromObject() to do the job.
                        // Throwing an exception is very unlikely, since we definitely have an
                        // ErlBinary or an ErlLazyBinary. We still call this function, because later
                        // a new binary representations can be introduced.
                        ErlBinary bin = ErlBinary.fromObject(top);

                        hash = hashBinaryBytes(bin, hash);
                        hash = hash * FUNNY_NUMBER4 + bin.getCompleteByteCount();

                        break;
                    }

                    case FUN: {

                        // The implementation of the functions are totally different than in the
                        // "original" ERTS. Thus, the hash value of a function is different.

                        ErlFunction fun = (ErlFunction) top;

                        hash = hash * FUNNY_NUMBER10 + fun.getArity();
                        hash = hash * FUNNY_NUMBER1 + ErlAtom.calcHashValue(fun.getModule());
                        hash = hash * FUNNY_NUMBER1 + ErlAtom.calcHashValue(fun.getName());

                        break;
                    }

                    default:
                        throw new RuntimeException("Term rank is not handled in makeHash(): " + rank);
                }

            } catch (TailRecur tr) {
                // just ignore it to continue the loop
            }
        }
    }

    private static long hashBinaryBytes(ErlBinary bin, long hash_) {

        long hash = hash_;
        final byte[] bytes = bin.toByteArray();
        final int len = bin.getCompleteByteCount();

        // Luckily, all constructed binaries are left-aligned. Thus, the bit-magic part of the
        // "original" implementation can be ignored.
        for (int i = 0; i < len; ++i) {
            hash = hash * FUNNY_NUMBER1 + Byte.toUnsignedLong(bytes[i]);
        }

        // The only remaining trick is here for the remaining byte fragment. There is no other way.
        if (bin.hasByteFragment()) {

            final int bitsize = bin.getUnusedBits();
            final long b = Byte.toUnsignedLong(bytes[bytes.length - 1]) >>> bitsize;

            hash = (hash * FUNNY_NUMBER1 + b) * FUNNY_NUMBER12 + (ErlBinary.BITS_PER_BYTE - bitsize);
        }

        return hash;
    }

    public static int makeBrokenHash(Object term) {
        // TODO
        return term.hashCode();
    }
}
