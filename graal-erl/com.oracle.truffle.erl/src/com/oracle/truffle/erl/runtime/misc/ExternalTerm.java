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

import java.io.ByteArrayOutputStream;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.util.zip.DataFormatException;
import java.util.zip.Deflater;
import java.util.zip.Inflater;

import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.ErlMap;
import com.oracle.truffle.erl.runtime.ErlTuple;

public final class ExternalTerm {

    public static final int NO_COMPRESSION = -1;
    public static final int DEFAULT_MINOR_VERSION = 1;
    public static final int DEFAULT_COMPRESSION_LEVEL = 6;

    public static final byte NEW_FLOAT_EXT = 70;
    public static final byte COMPRESSED_TAG = 80;
    public static final byte SMALL_INTEGER_EXT = 97;
    public static final byte INTEGER_EXT = 98;
    public static final byte FLOAT_EXT = 99;
    public static final byte ATOM_EXT = 100;
    public static final byte REFERENCE_EXT = 101;
    public static final byte PORT_EXT = 102;
    public static final byte PID_EXT = 103;
    public static final byte SMALL_TUPLE_EXT = 104;
    public static final byte LARGE_TUPLE_EXT = 105;
    public static final byte NIL_EXT = 106;
    public static final byte STRING_EXT = 107;
    public static final byte LIST_EXT = 108;
    public static final byte SMALL_BIG_EXT = 110;
    public static final byte LARGE_BIG_EXT = 111;
    public static final byte SMALL_ATOM_EXT = 115;
    public static final byte MAP_EXT = 116;
    public static final byte ATOM_UTF8_EXT = 118;
    public static final byte SMALL_ATOM_UTF8_EXT = 119;
    public static final byte EXT_TAG = (byte) 131;

    public static byte[] toBinary(Object term) {
        return toBinary(term, DEFAULT_MINOR_VERSION, NO_COMPRESSION);
    }

    public static byte[] toBinary(Object term, final int minorVersion) {
        return toBinary(term, minorVersion, NO_COMPRESSION);
    }

    public static byte[] toBinary(Object term, final int minorVersion, final boolean compressed) {
        return toBinary(term, minorVersion, compressed ? DEFAULT_COMPRESSION_LEVEL : NO_COMPRESSION);
    }

    public static byte[] toBinary(Object term, final int minorVersion, final int compressLevel) {

        if (NO_COMPRESSION == compressLevel) {

            return toRawBinary(term, minorVersion);

        } else {

            final byte[] raw = toRawBinary(term, minorVersion);
            final byte[] deflated = new byte[raw.length * 2];
            Deflater compresser = new Deflater(compressLevel);
            compresser.setInput(raw);
            compresser.finish();
            final int compSize = compresser.deflate(deflated);

            if (0 == compSize || deflated.length == compSize) {
                // 'deflated' buffer should not filled completely
                return null;
            }

            if (raw.length < compSize) {
                // if the compressed is larger than the raw, then return the raw
                return raw;
            }

            final byte[] result = new byte[6 + deflated.length];

            result[0] = EXT_TAG;
            result[1] = COMPRESSED_TAG;
            result[2] = (byte) (raw.length >>> 24);
            result[3] = (byte) ((raw.length >>> 16) & 0xff);
            result[4] = (byte) ((raw.length >>> 8) & 0xff);
            result[5] = (byte) (raw.length & 0xff);
            System.arraycopy(raw, 0, result, 6, raw.length);

            return result;
        }
    }

    public static Object fromBinary(byte[] data) {

        if (EXT_TAG != data[0]) {
            return null;
        }

        if (COMPRESSED_TAG == data[1]) {
            int uncompressedSize = 0;
            for (int i = 0; i < 4; ++i) {
                uncompressedSize = (uncompressedSize << 8) | Byte.toUnsignedInt(data[2 + i]);
            }

            final byte[] uncompressed = new byte[uncompressedSize];

            Inflater uncomp = new Inflater();
            uncomp.setInput(data, 6, data.length - 6);

            try {
                if (uncompressed.length != uncomp.inflate(uncompressed)) {
                    return null;
                }
            } catch (DataFormatException ex) {
                return null;
            }

            // the uncompressed data must be a regular, uncompressed serialized term
            if (EXT_TAG != uncompressed[0] || COMPRESSED_TAG == uncompressed[1]) {
                return null;
            }

            return new Decoder(ByteBuffer.wrap(uncompressed, 1, uncompressed.length - 1)).get();

        } else {

            return new Decoder(ByteBuffer.wrap(data, 1, data.length - 1)).get();
        }
    }

    private static byte[] toRawBinary(Object term, final int minorVersion) {

        ByteArrayOutputStream out = new ByteArrayOutputStream();

        if (!new Encoder(out, minorVersion).encode(term)) {
            // failed to encode
            return null;
        }

        byte[] result = new byte[1 + out.size()];

        result[0] = EXT_TAG;
        System.arraycopy(out.toByteArray(), 0, result, 1, out.size());

        return result;
    }

    public static final class Encoder {

        final ByteArrayOutputStream out;
        final int minorVersion;

        Encoder(ByteArrayOutputStream out, final int minorVersion) {
            this.out = out;
            this.minorVersion = minorVersion;
        }

        public boolean encode(Object term) {

            if (term instanceof Boolean) {
                return ErlAtom.fromBoolean((boolean) term).encode(out);
            } else if (term instanceof ErlAtom) {
                return ((ErlAtom) term).encode(out);
            } else if (term instanceof ErlTuple) {
                return ((ErlTuple) term).encode(out, this);
            } else if (term instanceof ErlList) {
                return ((ErlList) term).encode(out, this);
            } else if (term instanceof ErlMap) {
                return ((ErlMap) term).encode(out, this);
            } else if (term instanceof Long) {

                final long n = (long) term;

                if (0 <= n && n <= 255) {

                    out.write(SMALL_INTEGER_EXT);
                    out.write((int) (n & 0xff));

                    return true;

                } else if (Integer.MIN_VALUE <= n && n <= Integer.MAX_VALUE) {

                    final int i = (int) n;

                    out.write(INTEGER_EXT);
                    out.write((i >>> 24) & 0xff);
                    out.write((i >>> 16) & 0xff);
                    out.write((i >>> 8) & 0xff);
                    out.write(i & 0xff);

                    return true;

                } else {

                    return encodeBigInteger(BigInteger.valueOf(n));
                }

            } else if (term instanceof BigInteger) {

                return encodeBigInteger((BigInteger) term);

            } else if (term instanceof Double) {

                if (1 == minorVersion) {

                    ByteBuffer bb = ByteBuffer.allocate(Double.BYTES);
                    bb.putDouble((double) term);
                    out.write(NEW_FLOAT_EXT);
                    bb.position(0);
                    for (int i = 0; i < Double.BYTES; ++i) {
                        out.write(bb.get());
                    }
                    return true;

                } else if (0 == minorVersion) {

                    out.write(FLOAT_EXT);

                    final String str = String.format("%.20e", term);
                    for (int i = 0; i < str.length(); ++i) {
                        out.write(str.charAt(i));
                    }

                    for (int i = 0; i < 5; ++i) {
                        out.write(0);
                    }

                    return true;

                } else {
                    return false;
                }

            } else {
                return false;
            }
        }

        private boolean encodeBigInteger(BigInteger bi) {

            if (0 == bi.signum()) {
                return encode((long) 0);
            }

            final byte[] bytes = bi.abs().toByteArray();

            if (bytes.length <= 255) {

                out.write(SMALL_BIG_EXT);
                out.write(bytes.length);

            } else {

                out.write(LARGE_BIG_EXT);
                out.write((bytes.length >>> 24) & 0xff);
                out.write((bytes.length >>> 16) & 0xff);
                out.write((bytes.length >>> 8) & 0xff);
                out.write(bytes.length & 0xff);
            }

            if (0 > bi.signum()) {
                out.write(1);
            } else {
                out.write(0);
            }

            for (int i = bytes.length - 1; i >= 0; --i) {
                out.write(bytes[i]);
            }

            return true;
        }
    }

    private static final class Decoder {

        final ByteBuffer buf;

        Decoder(ByteBuffer buf) {
            this.buf = buf;
        }

        public Object get() {

            switch (buf.get()) {
                case SMALL_INTEGER_EXT:
                    return Byte.toUnsignedLong(buf.get());

                case INTEGER_EXT:
                    return (long) buf.getInt();

                case SMALL_BIG_EXT:
                    return getBigInteger(Byte.toUnsignedInt(buf.get()));

                case LARGE_BIG_EXT:
                    return getBigInteger(buf.getInt());

                case NEW_FLOAT_EXT:
                    return buf.getDouble();

                case FLOAT_EXT: {
                    StringBuilder sb = new StringBuilder();

                    char ch;
                    while ('\0' != (ch = (char) buf.get())) {
                        sb.append(ch);
                    }

                    while (buf.hasRemaining() && 0 == buf.get(buf.position())) {
                        buf.get();
                    }

                    return new Double(sb.toString());
                }

                case ATOM_EXT:
                    return getAtom(Short.toUnsignedInt(buf.getShort()), false);

                case SMALL_ATOM_EXT:
                    return getAtom(Byte.toUnsignedInt(buf.get()), false);

                case ATOM_UTF8_EXT:
                    return getAtom(Short.toUnsignedInt(buf.getShort()), true);

                case SMALL_ATOM_UTF8_EXT:
                    return getAtom(Byte.toUnsignedInt(buf.get()), true);

                case SMALL_TUPLE_EXT:
                    return getTuple(Byte.toUnsignedInt(buf.get()));

                case LARGE_TUPLE_EXT:
                    return getTuple(buf.getInt());

                case MAP_EXT:
                    return getMap(buf.getInt());

                case NIL_EXT:
                    return ErlList.NIL;

                case STRING_EXT:
                    return getString(Short.toUnsignedInt(buf.getShort()));

                case LIST_EXT:
                    return getList(buf.getInt());
            }

            return null;
        }

        private BigInteger getBigInteger(int len) {

            int sign = buf.get();

            final byte[] bytes = new byte[len];
            for (int i = len - 1; i >= 0; --i) {
                bytes[i] = buf.get();
            }

            if (0 == sign) {
                return new BigInteger(1, bytes);
            } else {
                return new BigInteger(-1, bytes);
            }
        }

        private Object getAtom(int len, boolean utf8) {

            if (utf8) {

                ByteBuffer bb = ByteBuffer.allocate(len);

                for (int i = 0; i < len; ++i) {
                    bb.put(buf.get());
                }

                try {
                    return ErlContext.UTF8_CHARSET.decode(bb).toString();
                } catch (Error ex) {
                    return null;
                }

            } else {

                StringBuilder sb = new StringBuilder();

                for (int i = 0; i < len; ++i) {
                    sb.append((char) buf.get());
                }

                return new ErlAtom(sb.toString());
            }
        }

        private ErlTuple getTuple(int arity) {

            Object[] elems = new Object[arity];

            for (int i = 0; i < arity; ++i) {
                elems[i] = get();
            }

            return ErlTuple.fromArray(elems);
        }

        private ErlMap getMap(int arity) {

            ErlMap.Assoc[] elems = new ErlMap.Assoc[arity];

            for (int i = 0; i < arity; ++i) {
                final Object key = get();
                final Object val = get();
                elems[i] = new ErlMap.Assoc(key, val);
            }

            return ErlMap.fromArray(elems);
        }

        private ErlList getString(int length) {

            Object[] elems = new Object[length];

            for (int i = 0; i < length; ++i) {
                elems[i] = Byte.toUnsignedLong(buf.get());
            }

            return ErlList.fromArray(elems);
        }

        private Object getList(int length) {

            Object[] elems = new Object[length];

            for (int i = 0; i < length; ++i) {
                elems[i] = get();
            }

            Object list = get();

            for (int i = length - 1; i >= 0; --i) {
                list = new ErlList(elems[i], list);
            }

            return list;
        }
    }
}
