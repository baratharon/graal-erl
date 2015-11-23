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

import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlTuple;
import com.oracle.truffle.erl.runtime.misc.CharDataVisitor;

/**
 * Convert chardata() and other similar things to unicode codepoint list or UTF-8 binary.
 * <p>
 * Also supplies some constants and functions for other conversions.
 */
public final class Unicode {

    public static final int UTF_MAX_VALUE = 0x10ffff;
    public static final int UTF_RESERVED_START = 0xD800;
    public static final int UTF_RESERVED_END = 0xDFFF;

    public static final int UTF16_HIGH_SURROGATE = 0xD800;
    public static final int UTF16_LOW_SURROGATE = 0xDC00;
    public static final int UTF16_DATA_BITS = 10;
    public static final int UTF16_DATA_MASK = 0x3FF;

    public static final int UTF8_LAST_1 = 0x7F;
    public static final int UTF8_LAST_2 = 0x7FF;
    public static final int UTF8_LAST_3 = 0xFFFF;
    public static final int UTF8_LAST_4 = 0x1FFFFF;
    public static final int UTF8_CSEQ = 0x80; // sequence continue
    public static final int UTF8_CSEQ_BITS = 6;
    public static final int UTF8_CSEQ_DATA_MASK = 0x3F;
    public static final int UTF8_SEQ2 = 0xC0;
    public static final int UTF8_SEQ2_BITS = 5;
    public static final int UTF8_SEQ2_DATA_MASK = 0x1F;
    public static final int UTF8_SEQ3 = 0xE0;
    public static final int UTF8_SEQ3_BITS = 4;
    public static final int UTF8_SEQ3_DATA_MASK = 0xF;
    public static final int UTF8_SEQ4 = 0xF0;
    public static final int UTF8_SEQ4_BITS = 3;
    public static final int UTF8_SEQ4_DATA_MASK = 0x7;

    public static interface CodepointCallback {
        public void codepoint(int codepoint);
    }

    private static final class CharDataDecoderVisitor extends CharDataVisitor {

        private final Decoder decoder;
        private final CodepointCallback callback;
        private boolean byteMode = false;

        public CharDataDecoderVisitor(Decoder decoder, CodepointCallback callback) {
            this.decoder = decoder;
            this.callback = callback;
        }

        @Override
        protected void visit(long l) {
            if (byteMode) {
                if (!decoder.isComplete()) {
                    throw DecodeFailedException.INSTANCE;
                }
                byteMode = false;
            }

            if ((0 <= l && l < UTF_RESERVED_START) || (UTF_RESERVED_END < l && l <= UTF_MAX_VALUE)) {
                codepoint((int) l);
            } else {
                throw DecodeFailedException.INSTANCE;
            }
        }

        @Override
        protected void visit(byte b) {
            byteMode = true;
            decoder.feed(b);

            if (decoder.isComplete()) {
                codepoint(decoder.getCodepoint());
            }
        }

        protected void codepoint(int cp) {
            callback.codepoint(cp);
        }
    }

    public enum Encoding {
        LATIN1,
        UTF8,
        UTF16_BIG,
        UTF16_LITTLE,
        UTF32_BIG,
        UTF32_LITTLE
    }

    public static Decoder getDecoder(Encoding encoding) {

        if (null == encoding) {
            return null;
        }

        switch (encoding) {
            case LATIN1:
                return new Latin1Decoder();

            case UTF8:
                return new UTF8Decoder();

            case UTF16_BIG:
                return new UTF16Decoder(true);

            case UTF16_LITTLE:
                return new UTF16Decoder(false);

            case UTF32_BIG:
                return new UTF32Decoder(true);

            case UTF32_LITTLE:
                return new UTF32Decoder(false);

            default:
                return null;
        }
    }

    public static Encoder getEncoder(Encoding encoding) {

        if (null == encoding) {
            return null;
        }

        switch (encoding) {
            case LATIN1:
                return new Latin1Encoder();

            case UTF8:
                return new UTF8Encoder();

            case UTF16_BIG:
                return new UTF16Encoder(true);

            case UTF16_LITTLE:
                return new UTF16Encoder(false);

            case UTF32_BIG:
                return new UTF32Encoder(true);

            case UTF32_LITTLE:
                return new UTF32Encoder(false);

            default:
                return null;
        }
    }

    private static Encoding parseEncoding(Object inEncoding) {

        if (ErlAtom.LATIN1.equals(inEncoding)) {
            return Encoding.LATIN1;
        } else if (ErlAtom.UTF8.equals(inEncoding) || ErlAtom.UNICODE.equals(inEncoding)) {
            return Encoding.UTF8;
        } else if (ErlAtom.UTF16.equals(inEncoding)) {
            return Encoding.UTF16_BIG;
        } else if (ErlAtom.UTF32.equals(inEncoding)) {
            return Encoding.UTF32_BIG;
        } else {

            if (inEncoding instanceof ErlTuple) {
                ErlTuple tuple = (ErlTuple) inEncoding;

                if (2 != tuple.getSize()) {
                    return null;
                }

                final Object encode = tuple.getElement(1);
                final Object endian = tuple.getElement(2);

                boolean big;

                if (ErlAtom.BIG.equals(endian)) {
                    big = true;
                } else if (ErlAtom.LITTLE.equals(endian)) {
                    big = false;
                } else {
                    return null;
                }

                if (ErlAtom.UTF16.equals(encode)) {

                    if (big) {
                        return Encoding.UTF16_BIG;
                    } else {
                        return Encoding.UTF16_LITTLE;
                    }

                } else if (ErlAtom.UTF32.equals(encode)) {

                    if (big) {
                        return Encoding.UTF32_BIG;
                    } else {
                        return Encoding.UTF32_LITTLE;
                    }
                }
            }
        }

        return null;
    }

    public static boolean convert(CodepointCallback cb, Object data, Object inEncoding) {

        final Encoding encoding = parseEncoding(inEncoding);

        if (null == encoding) {
            return false;
        }

        final Decoder decoder = getDecoder(encoding);
        final CharDataVisitor vis = new CharDataDecoderVisitor(decoder, cb);

        try {
            vis.accept(data);
            return true;
        } catch (DecodeFailedException ex) {
            return false;
        }
    }
}
