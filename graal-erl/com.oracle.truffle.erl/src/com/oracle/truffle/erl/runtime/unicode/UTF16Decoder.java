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
package com.oracle.truffle.erl.runtime.unicode;

public final class UTF16Decoder extends DecoderHelper {

    private final int highShift;
    private final int lowShift;
    private int highWord = 0;
    private int word = 0;
    private int more;
    private boolean second;
    private boolean sequence;

    public UTF16Decoder(boolean bigendian) {
        super();

        if (bigendian) {
            highShift = 8;
            lowShift = 0;
        } else {
            highShift = 0;
            lowShift = 8;
        }

        reset();
    }

    public void reset() {
        second = sequence = false;
        more = 2;
    }

    public int more() {
        return more;
    }

    @Override
    protected void processByte(int ub) {

        if (second) {
            word |= ub << lowShift;
        } else {
            word = ub << highShift;
        }

        second = !second;

        if (0 == more) {
            more = 2;
        } else {
            --more;
        }

        // parse sequence

        if (!second) {
            if (sequence) {

                sequence = false;

                if (Unicode.UTF16_LOW_SURROGATE <= word && word <= Unicode.UTF_RESERVED_END) {
                    codepointFound(0x10000 + (((highWord & Unicode.UTF16_DATA_MASK) << Unicode.UTF16_DATA_BITS) | (word & Unicode.UTF16_DATA_MASK)));
                } else {
                    throw DecodeFailedException.INSTANCE;
                }

            } else if (Unicode.UTF16_HIGH_SURROGATE <= word && word < Unicode.UTF16_LOW_SURROGATE) {

                // wait for the second part of the sequence
                sequence = true;
                highWord = word;
                more += 2;

            } else {

                // not a sequence
                codepointFound(word);
            }
        }
    }

    public boolean isComplete() {
        return !second && !sequence;
    }
}
