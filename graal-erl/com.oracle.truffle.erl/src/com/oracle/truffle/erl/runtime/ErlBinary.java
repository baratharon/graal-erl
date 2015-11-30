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
package com.oracle.truffle.erl.runtime;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.List;

import com.oracle.truffle.api.interop.ForeignAccess;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;

/**
 * The Erlang binaries and bit strings are represented as array of bytes.
 */
public final class ErlBinary implements TruffleObject {

    final private int unusedBits;
    final private byte[] bytes;

    public static final int BITS_PER_BYTE = 8;
    public static final ErlBinary EMPTY = new ErlBinary(0);

    public ErlBinary(int unusedBits, byte... bytes) {

        this.unusedBits = unusedBits;
        this.bytes = bytes;

        assert null != bytes;
    }

    public static ErlBinary fromString(final String str, Charset charset) {
        ByteBuffer bb = charset.encode(str);
        return new ErlBinary(0, Arrays.copyOf(bb.array(), bb.limit()));
    }

    public static ErlBinary fromArray(byte[] bytes) {
        return new ErlBinary(0, bytes);
    }

    public static ErlBinary fromArray(int unusedBits, byte[] bytes) {
        assert 0 <= unusedBits && unusedBits < ErlBinary.BITS_PER_BYTE;
        return new ErlBinary(unusedBits, bytes);
    }

    public static ErlBinary fromObject(Object obj) {

        if (obj instanceof ErlBinary) {
            return (ErlBinary) obj;
        }

        if (obj instanceof ErlLazyBinary) {
            return ((ErlLazyBinary) obj).construct();
        }

        if (obj instanceof ErlBinaryView) {
            return ((ErlBinaryView) obj).construct();
        }

        throw ErlControlException.makeBadarg();
    }

    private final static class Builder {

        private byte[] bytes;
        private int byteIndex = 0;
        private int unusedBits = 0;
        private ErlBinary binary;

        public Builder(List<ErlBinary> bins) {

            int numBits = 0;

            for (ErlBinary bin : bins) {
                numBits += bin.getBitSize();
            }

            bytes = new byte[(numBits + BITS_PER_BYTE - 1) / BITS_PER_BYTE];

            for (ErlBinary bin : bins) {
                append(bin);
            }

            binary = new ErlBinary(unusedBits, bytes);
            bytes = null;
        }

        public Builder(ErlBinary[] bins) {

            int numBits = 0;

            for (ErlBinary bin : bins) {
                numBits += bin.getBitSize();
            }

            bytes = new byte[(numBits + BITS_PER_BYTE - 1) / BITS_PER_BYTE];

            for (ErlBinary bin : bins) {
                append(bin);
            }

            binary = new ErlBinary(unusedBits, bytes);
            bytes = null;
        }

        public ErlBinary getBinary() {
            return binary;
        }

        private void append(ErlBinary bin) {

            if (0 == unusedBits) {

                for (byte b : bin.bytes) {

                    bytes[byteIndex++] = b;
                }

                unusedBits = bin.unusedBits;

            } else {

                final int usedBits = BITS_PER_BYTE - unusedBits;
                byte lastByte = bytes[--byteIndex];

                for (byte b : bin.bytes) {

                    final int ub = Byte.toUnsignedInt(b);
                    bytes[byteIndex++] = (byte) (lastByte | (ub >>> usedBits));
                    lastByte = (byte) (ub << unusedBits);
                }

                final int cumulativeUnusedBits = unusedBits + bin.unusedBits;
                assert cumulativeUnusedBits < 2 * BITS_PER_BYTE;

                if (cumulativeUnusedBits < BITS_PER_BYTE) {

                    bytes[byteIndex++] = lastByte;
                    unusedBits = cumulativeUnusedBits;

                } else {

                    // if the cumulative unused bits are 8 or more, then the last byte contains no
                    // relevant data, and must be ignored
                    unusedBits = cumulativeUnusedBits - BITS_PER_BYTE;
                }
            }
        }
    }

    public static ErlBinary concatenate(ErlBinary[] bins) {

        return new Builder(bins).getBinary();
    }

    public static ErlBinary concatenate(List<ErlBinary> bins) {

        return new Builder(bins).getBinary();
    }

    public final byte[] toByteArray() {
        return bytes;
    }

    public final byte[] extract(final int bitOffset, final int bitSize, final boolean leftAligned) {
        if (leftAligned) {
            return extractLeft(bitOffset, bitSize);
        } else {
            return extractRight(bitOffset, bitSize);
        }
    }

    public final byte[] extractLeft(final int bitOffset, final int bitSize) {

        // System.out.println("L " + bitSize + "@" + bitOffset);

        final int binaryBitSize = bytes.length * BITS_PER_BYTE - unusedBits;

        if (0 == bitOffset && bitSize == binaryBitSize) {
            return bytes;
        }

        if ((bitOffset + bitSize) > binaryBitSize) {
            throw ErlControlException.makeBadmatch(this);
        }

        if (0 == bitSize) {
            return new byte[0];
        }

        final int resultByteSize = (bitSize + BITS_PER_BYTE - 1) / BITS_PER_BYTE;
        final int resultUsedBits = bitSize & (BITS_PER_BYTE - 1);
        final int resultUnusedBits = (0 == resultUsedBits) ? 0 : (BITS_PER_BYTE - resultUsedBits);
        final int sourceByteStart = bitOffset / BITS_PER_BYTE;
        final int sourceByteEnd = Integer.min((bitOffset + bitSize) / BITS_PER_BYTE, bytes.length - 1);
        final int readLeftShift = bitOffset & (BITS_PER_BYTE - 1);
        final int readRightShift = BITS_PER_BYTE - readLeftShift;

        // System.out.println(" rBS=" + resultByteSize + ", rUB=" + resultUsedBits + ", rUnB=" +
        // resultUnusedBits + ", sBS=" + sourceByteStart + ", sBE=" + sourceByteEnd + ", rLS=" +
        // readLeftShift +
        // ", rRS=" + readRightShift);

        byte[] result;

        if (0 == readLeftShift) {

            // System.out.println(" aligned");
            result = Arrays.copyOfRange(bytes, sourceByteStart, sourceByteStart + resultByteSize);

        } else {

            result = new byte[resultByteSize];

            // process the first byte

            // System.out.println(" F " + Byte.toUnsignedInt(bytes[sourceByteStart]) + "@" +
            // sourceByteStart + " -> " + (Byte.toUnsignedInt(bytes[sourceByteStart]) <<
            // readLeftShift) + " | " +
            // (Byte.toUnsignedInt(bytes[sourceByteStart]) >>> readRightShift));
            result[0] = (byte) (Byte.toUnsignedInt(bytes[sourceByteStart]) << readLeftShift);

            // process all intermediate bytes

            int sourceByteIndex = sourceByteStart + 1;
            int resultByteIndex = 0;

            while (sourceByteIndex <= sourceByteEnd) {

                final int ub = Byte.toUnsignedInt(bytes[sourceByteIndex++]);

                // System.out.println(" I " + ub + "@" + (sourceByteIndex - 1) + " -> " + (ub <<
                // readLeftShift) + " | " + (ub >>> readRightShift));

                result[resultByteIndex++] |= ub >>> readRightShift;

                if (resultByteIndex < result.length) {
                    result[resultByteIndex] = (byte) (ub << readLeftShift);
                }
            }

            // process the last byte

            if (sourceByteEnd < bytes.length) {
                // System.out.println(" L " + Byte.toUnsignedInt(bytes[sourceByteEnd]) + "@" +
                // sourceByteEnd + " -> " + (Byte.toUnsignedInt(bytes[sourceByteEnd]) <<
                // readLeftShift) + " | " +
                // (Byte.toUnsignedInt(bytes[sourceByteEnd]) >>> readRightShift));
                // if (resultByteIndex < result.length) {
                result[result.length - 1] |= Byte.toUnsignedInt(bytes[sourceByteEnd]) >>> readRightShift;
                // }
            }
        }

        if (0 != resultUnusedBits) {

            // mask out the unnecessary bits
            result[result.length - 1] &= 0xff << resultUnusedBits;
        }

        return result;
    }

    public final byte[] extractRight(final int bitOffset, final int bitSize) {

        // System.out.println("R " + bitSize + "@" + bitOffset);

        final int binaryBitSize = bytes.length * BITS_PER_BYTE - unusedBits;

        if (0 == bitOffset && bitSize == binaryBitSize && 0 == (bitSize & (BITS_PER_BYTE - 1))) {
            return bytes;
        }

        if ((bitOffset + bitSize) > binaryBitSize) {
            throw ErlControlException.makeBadmatch(this);
        }

        if (0 == bitSize) {
            return new byte[0];
        }

        final int resultByteSize = (bitSize + BITS_PER_BYTE - 1) / BITS_PER_BYTE;
        final int resultUsedBits = bitSize & (BITS_PER_BYTE - 1);
        final int resultUnusedBits = (0 == resultUsedBits) ? 0 : (BITS_PER_BYTE - resultUsedBits);
        final int sourceByteStart = bitOffset / BITS_PER_BYTE;
        final int sourceByteEnd = (bitOffset + bitSize) / BITS_PER_BYTE;
        final int readLeftShift = (bitOffset + bitSize) & (BITS_PER_BYTE - 1);
        final int readRightShift = BITS_PER_BYTE - readLeftShift;

        // System.out.println(" rBS=" + resultByteSize + ", rUB=" + resultUsedBits + ", rUnB=" +
        // resultUnusedBits + ", sBS=" + sourceByteStart + ", sBE=" + sourceByteEnd + ", rLS=" +
        // readLeftShift +
        // ", rRS=" + readRightShift);

        byte[] result;

        if (0 == readLeftShift) {

            // aligned to byte at the end
            // System.out.println(" aligned");

            result = Arrays.copyOfRange(bytes, sourceByteStart, sourceByteStart + resultByteSize);

        } else {

            result = new byte[resultByteSize];

            // process the first byte

            // System.out.println(" F " + Byte.toUnsignedInt(bytes[sourceByteEnd]) + "@" +
            // sourceByteEnd + " -> " + (Byte.toUnsignedInt(bytes[sourceByteEnd]) << readLeftShift)
            // + " | " +
            // (Byte.toUnsignedInt(bytes[sourceByteEnd]) >>> readRightShift));
            result[resultByteSize - 1] = (byte) (Byte.toUnsignedInt(bytes[sourceByteEnd]) >>> readRightShift);

            // process all intermediate bytes

            int sourceByteIndex = sourceByteEnd - 1;
            int resultByteIndex = resultByteSize - 1;

            while (sourceByteIndex >= sourceByteStart) {

                final int ub = Byte.toUnsignedInt(bytes[sourceByteIndex--]);

                // System.out.println(" I " + ub + "@" + (sourceByteIndex + 1) + " -> " + (ub <<
                // readLeftShift) + " | " + (ub >>> readRightShift));

                result[resultByteIndex--] |= ub << readLeftShift;

                if (resultByteIndex >= 0) {
                    result[resultByteIndex] = (byte) (ub >>> readRightShift);
                }
            }

            // process the last byte

            // System.out.println(" L " + Byte.toUnsignedInt(bytes[sourceByteStart]) + "@" +
            // sourceByteStart + " -> " + (Byte.toUnsignedInt(bytes[sourceByteStart]) <<
            // readLeftShift) + " | " +
            // (Byte.toUnsignedInt(bytes[sourceByteStart]) >>> readRightShift));
            result[0] |= Byte.toUnsignedInt(bytes[sourceByteStart]) << readLeftShift;
        }

        if (0 != resultUnusedBits) {

            // mask out the unnecessary bits
            result[0] &= (1 << resultUsedBits) - 1;
        }

        return result;
    }

    @Override
    public String toString() {

        StringBuilder sbRaw = new StringBuilder();
        StringBuilder sbStr = (0 == unusedBits && 0 != bytes.length) ? new StringBuilder() : null;
        String sep = "";
        char[] refChar = new char[1];

        sbRaw.append("<<");
        if (null != sbStr) {
            sbStr.append("<<\"");
        }

        final int byteCount = getCompleteByteCount();

        for (int i = 0; i < byteCount; ++i) {
            sbRaw.append(sep);
            sbRaw.append(Byte.toUnsignedInt(bytes[i]));
            sep = ",";

            if (null != sbStr) {
                if (ErlContext.isPrintableCharacter(bytes[i], refChar, true)) {
                    sbStr.append(ErlContext.stringifyPrintableCharacter(refChar[0]));
                } else {
                    sbStr = null;
                }
            }
        }

        if (0 != unusedBits) {

            assert byteCount == bytes.length - 1;

            sbRaw.append(sep);
            sbRaw.append(Byte.toUnsignedInt(bytes[byteCount]) >>> unusedBits);
            sbRaw.append(':');
            sbRaw.append(BITS_PER_BYTE - unusedBits);
            sbStr = null;
        }

        if (null != sbStr) {
            return sbStr.append("\">>").toString();
        }

        sbRaw.append(">>");

        return sbRaw.toString();
    }

    /**
     * Returns the number of complete bytes. It can return with (bytes.length) or with
     * (bytes.length-1).
     */
    public int getCompleteByteCount() {
        return (0 == unusedBits) ? bytes.length : (bytes.length - 1);
    }

    public boolean hasByteFragment() {
        return 0 != unusedBits;
    }

    public int getBitSize() {
        return bytes.length * BITS_PER_BYTE - unusedBits;
    }

    public int getByteSize() {
        return bytes.length;
    }

    public int getUnusedBits() {
        return unusedBits;
    }

    public int compare(ErlBinary rhs) {

        final ErlBinary lhs = this;
        final int length = lhs.getBitSize();

        if (length < rhs.getBitSize()) {
            return -1;
        } else if (length > rhs.getBitSize()) {
            return 1;
        }

        for (int i = 0; i < lhs.bytes.length; ++i) {

            // no comment -- java does not support unsigned primitive types, but an "unsigned byte"
            // would be graceful here
            final int result = Integer.compare(Byte.toUnsignedInt(lhs.bytes[i]), Byte.toUnsignedInt(rhs.bytes[i]));

            if (0 != result) {
                return result;
            }
        }

        return 0;

    }

    @Override
    public ForeignAccess getForeignAccess() {
        return ErlFunctionForeignAccess.create();
    }
}
