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

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlList;

public final class IntegerConvert {

    // don't instantiate this
    private IntegerConvert() {
    }

    @TruffleBoundary
    public static Object listToInteger(ErlList list_, final BigInteger biBase, final long lBase) {

        if (lBase < 2 || lBase > 36) {
            throw ErlControlException.makeBadarg();
        }

        BigInteger bi = BigInteger.valueOf(0);
        ErlList list = list_;
        boolean negative = false;

        // NOTE: if lBase is less than 11, then we get an invalid range for the lower-case and
        // upper-case letters, which is good enough, so we don't have to slow down the code with
        // additional (and practically pointless) checks

        char maxNumChar = (lBase < 10) ? ((char) ('0' + lBase - 1)) : '9';
        char maxLetLower = (char) ('a' + lBase - 11);
        char maxLetUpper = (char) ('A' + lBase - 11);

        {
            long c = ErlContext.decodeLong(list.getHead());

            if ('-' == c) {
                negative = true;
                list = list.getTailList();

                if (ErlList.NIL == list) {
                    throw ErlControlException.makeBadarg();
                }
            }
        }

        while (ErlList.NIL != list) {
            long c = ErlContext.decodeLong(list.getHead());

            if ('0' <= c && c <= maxNumChar) {
                bi = bi.multiply(biBase).add(BigInteger.valueOf(c - '0'));
            } else if ('a' <= c && c <= maxLetLower) {
                bi = bi.multiply(biBase).add(BigInteger.valueOf(c - 'a' + 10));
            } else if ('A' <= c && c <= maxLetUpper) {
                bi = bi.multiply(biBase).add(BigInteger.valueOf(c - 'A' + 10));
            } else {
                throw ErlControlException.makeBadarg();
            }

            list = list.getTailList();
        }

        if (negative) {
            bi = bi.negate();
        }

        // try to simplify the BigInteger to long
        try {
            return bi.longValueExact();
        } catch (ArithmeticException ex) {
            return bi;
        }
    }

    @TruffleBoundary
    public static ErlList integerToList(final long numToConvert, final BigInteger biBase, final long lBase) {
        // TODO: the first approach is to use the BigInteger version; can be changed later in need
        return integerToList(BigInteger.valueOf(numToConvert), biBase, lBase);
    }

    @TruffleBoundary
    public static ErlList integerToList(final BigInteger numToConvert, final BigInteger biBase, final long lBase) {

        if (lBase < 2 || lBase > 36) {
            throw ErlControlException.makeBadarg();
        }

        BigInteger num = numToConvert;
        boolean negative = false;
        ErlList result = ErlList.NIL;

        if (num.signum() < 0) {
            negative = true;
            num = num.negate();
        }

        do {

            BigInteger[] divrem = num.divideAndRemainder(biBase);
            num = divrem[0];

            // we can convert to int without any condition, because it MUST be between 0 and (lBase
            // - 1).
            final int digit = divrem[1].intValue();
            long elem;

            if (digit <= 9) {
                elem = '0' + digit;
            } else {
                elem = 'A' + digit - 10;
            }

            result = new ErlList(elem, result);

        } while (0 < num.signum());

        if (negative) {
            result = new ErlList((long) '-', result);
        }

        return result;
    }
}
