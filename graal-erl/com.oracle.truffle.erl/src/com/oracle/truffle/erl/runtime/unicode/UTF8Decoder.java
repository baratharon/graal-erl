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

public final class UTF8Decoder extends DecoderHelper {

    private int code;
    private int more;
    private boolean first;
    private boolean complete;

    public UTF8Decoder() {
        super();

        reset();
    }

    public void reset() {
        more = 1;
        code = 0;
        complete = false;
        first = true;
    }

    public int more() {
        return more;
    }

    public boolean isComplete() {
        return complete;
    }

    @Override
    protected void processByte(int ub) {
        if (complete) {
            reset();
        }

        --more;

        if (first) {

            first = false;

            if (ub < Unicode.UTF8_LAST_1) {
                complete = true;
                codepointFound(ub);
            } else if ((ub & ~Unicode.UTF8_SEQ2_DATA_MASK) == Unicode.UTF8_SEQ2) {
                code = ub & Unicode.UTF8_SEQ2_DATA_MASK;
                more = 1;
            } else if ((ub & ~Unicode.UTF8_SEQ3_DATA_MASK) == Unicode.UTF8_SEQ3) {
                code = ub & Unicode.UTF8_SEQ3_DATA_MASK;
                more = 2;
            } else if ((ub & ~Unicode.UTF8_SEQ4_DATA_MASK) == Unicode.UTF8_SEQ4) {
                code = ub & Unicode.UTF8_SEQ4_DATA_MASK;
                more = 3;
            } else {
                throw DecodeFailedException.INSTANCE;
            }

        } else {

            code <<= Unicode.UTF8_CSEQ_BITS;
            code |= (ub & Unicode.UTF8_CSEQ_DATA_MASK);

            if (0 == more) {
                complete = true;
                codepointFound(code);
            }
        }
    }
}
