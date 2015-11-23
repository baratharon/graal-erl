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

public final class UTF16Encoder extends EncoderHelper {

    private final int highShift;
    private final int lowShift;

    public UTF16Encoder(boolean bigendian) {
        super(4);

        if (bigendian) {
            highShift = 8;
            lowShift = 0;
        } else {
            highShift = 0;
            lowShift = 8;
        }
    }

    @Override
    protected int encode(byte[] buf, int codepoint) {

        if (codepoint <= 0xffff) {

            buf[0] = (byte) (codepoint >>> highShift);
            buf[1] = (byte) (codepoint >>> lowShift);
            return 2;

        } else {

            final int code = codepoint - 0x10000;
            final int upper_half = Unicode.UTF16_HIGH_SURROGATE | (code >>> 10);
            final int lower_half = Unicode.UTF16_LOW_SURROGATE | (code & Unicode.UTF16_DATA_MASK);

            buf[0] = (byte) (upper_half >>> highShift);
            buf[1] = (byte) (upper_half >>> lowShift);
            buf[2] = (byte) (lower_half >>> highShift);
            buf[3] = (byte) (lower_half >>> lowShift);
            return 4;
        }
    }
}
