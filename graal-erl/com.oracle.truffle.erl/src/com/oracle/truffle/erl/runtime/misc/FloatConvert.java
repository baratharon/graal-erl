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
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.ErlTuple;

public final class FloatConvert {

    // don't instantiate this
    private FloatConvert() {
    }

    public static ErlList toList(double value, ErlList options) {

        boolean sci = true;
        int precision = 20;
        boolean compact = false;

        for (ErlList opt = options; ErlList.NIL != opt; opt = opt.getTailList()) {

            final Object head = opt.getHead();

            if (ErlAtom.COMPACT.equals(head)) {

                compact = true;
                continue;

            } else if (head instanceof ErlTuple) {

                final ErlTuple tuple = (ErlTuple) head;

                if (2 == tuple.getSize()) {

                    // There is no risk: the second element MUST be an integer. If it is not, we
                    // will throw badarg anyway.
                    precision = ErlContext.decodeInt(tuple.getElement(2));

                    if (ErlAtom.DECIMALS.equals(tuple.getElement(1))) {
                        sci = false;
                        continue;

                    } else if (ErlAtom.SCIENTIFIC.equals(tuple.getElement(1))) {
                        sci = true;
                        continue;
                    }
                }
            }

            throw ErlControlException.makeBadarg();
        }

        final String format;

        if (sci) {
            format = "%." + precision + "e";
        } else {
            format = "%." + precision + "f";
        }

        String string = String.format(format, value);

        if (compact) {

            // faster than a regex match
            int newLength = string.length();

            while (newLength > 2 && '0' == string.charAt(newLength - 1) && '.' != string.charAt(newLength - 2)) {
                --newLength;
            }

            string = string.substring(0, newLength);
        }

        return ErlList.fromString(string);
    }

}
