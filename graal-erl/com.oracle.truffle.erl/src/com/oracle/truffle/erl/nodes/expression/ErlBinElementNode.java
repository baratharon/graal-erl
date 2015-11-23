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
package com.oracle.truffle.erl.nodes.expression;

import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.nodes.ErlExpressionNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlBinary;
import com.oracle.truffle.erl.runtime.ErlBinaryView;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlLazyBinary;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.unicode.DecodeFailedException;
import com.oracle.truffle.erl.runtime.unicode.Decoder;
import com.oracle.truffle.erl.runtime.unicode.EncodeFailedException;
import com.oracle.truffle.erl.runtime.unicode.Encoder;
import com.oracle.truffle.erl.runtime.unicode.Unicode;

/**
 * Describes an element (a segment) of a binary.
 * <p>
 * <ul>
 * <li>The default type for a segment is integer. The default type does not depend on the value,
 * even if the value is a literal. For example, the default type in <<3.14>> is integer, not float.
 *
 * <li>The default Size depends on the type. For integer it is 8. For float it is 64. For binary it
 * is all of the binary. In matching, this default value is only valid for the last element. All
 * other binary elements in matching must have a size specification.
 *
 * <li>The default unit depends on the the type. For integer, float, and bitstring it is 1. For
 * binary it is 8.
 *
 * <li>The default signedness is unsigned.
 *
 * <li>The default endianness is big.
 * </ul>
 */
@NodeInfo(shortName = "bin_element")
public final class ErlBinElementNode extends ErlExpressionNode {

    @Child private ErlExpressionNode valueExprNode;
    @Child private ErlExpressionNode sizeExprNode;
    private final int unitSize;
    private final boolean alwaysBadarg;
    private final boolean needByteOrderSwap;
    private final boolean isSigned;
    private final TypeSpecifier typeSpec;
    private final Encoder encoder;
    private final Decoder decoder;

    public ErlBinElementNode(SourceSection src, ErlExpressionNode valueExprNode, ErlExpressionNode sizeExprNode, TypeSpecifier[] specifiers) {
        super(src);
        this.valueExprNode = valueExprNode;
        this.sizeExprNode = sizeExprNode;
        this.unitSize = findUnitSize(specifiers);
        this.isSigned = haveSpecifier(TypeSpecifier.SIGNED, specifiers);

        boolean alwaysBad = false;

        // If both SIGNED and UNSIGNED are set, we need to throw makeBadarg().
        if (haveSpecifier(TypeSpecifier.SIGNED, specifiers) && haveSpecifier(TypeSpecifier.UNSIGNED, specifiers)) {
            alwaysBad = true;
        }

        // Process byte order, to find out whether need to swap bytes.
        // Note, ByteBuffer is used in big endian mode.
        {
            boolean needSwap = false;
            int specifiersFound = 0;

            if (haveSpecifier(TypeSpecifier.BIG, specifiers)) {
                ++specifiersFound;
            }
            if (haveSpecifier(TypeSpecifier.LITTLE, specifiers)) {
                ++specifiersFound;
                needSwap = true;
            }
            if (haveSpecifier(TypeSpecifier.NATIVE, specifiers)) {
                ++specifiersFound;
                needSwap = ErlContext.IS_LITTLE_ENDIAN;
            }

            if (1 < specifiersFound) {
                alwaysBad = true;
            }

            this.needByteOrderSwap = needSwap;
        }

        // Process type specifier. Default type is INTEGER.
        // If more than one type specifier found, then we will always throw a makeBadarg().
        {
            TypeSpecifier type = TypeSpecifier.INTEGER;
            int specifiersFound = 0;

            if (haveSpecifier(TypeSpecifier.INTEGER, specifiers)) {
                type = TypeSpecifier.INTEGER;
                ++specifiersFound;
            }
            if (haveSpecifier(TypeSpecifier.FLOAT, specifiers)) {
                type = TypeSpecifier.FLOAT;
                ++specifiersFound;
            }
            if (haveSpecifier(TypeSpecifier.BINARY, specifiers)) {
                type = TypeSpecifier.BINARY;
                ++specifiersFound;
            }
            if (haveSpecifier(TypeSpecifier.BITSTRING, specifiers)) {
                type = TypeSpecifier.BITSTRING;
                ++specifiersFound;
            }
            if (haveSpecifier(TypeSpecifier.UTF8, specifiers)) {
                type = TypeSpecifier.UTF8;
                ++specifiersFound;
            }
            if (haveSpecifier(TypeSpecifier.UTF16, specifiers)) {
                type = TypeSpecifier.UTF16;
                ++specifiersFound;
            }
            if (haveSpecifier(TypeSpecifier.UTF32, specifiers)) {
                type = TypeSpecifier.UTF32;
                ++specifiersFound;
            }

            if (1 < specifiersFound) {
                alwaysBad = true;
            }

            this.typeSpec = type;
        }

        Unicode.Encoding encoding;

        if (TypeSpecifier.UTF32 == typeSpec) {
            encoding = needByteOrderSwap ? Unicode.Encoding.UTF32_LITTLE : Unicode.Encoding.UTF32_BIG;
        } else if (TypeSpecifier.UTF16 == typeSpec) {
            encoding = needByteOrderSwap ? Unicode.Encoding.UTF16_LITTLE : Unicode.Encoding.UTF16_BIG;
        } else if (TypeSpecifier.UTF8 == typeSpec) {
            encoding = Unicode.Encoding.UTF8;
        } else {
            encoding = null;
        }

        // will return null when 'encoding' is 'null'
        encoder = Unicode.getEncoder(encoding);
        decoder = Unicode.getDecoder(encoding);

        this.alwaysBadarg = alwaysBad;
        assert null != valueExprNode;
    }

    public static final class TypeSpecifier {

        public static final TypeSpecifier LITTLE = new TypeSpecifier();
        public static final TypeSpecifier BIG = new TypeSpecifier();
        public static final TypeSpecifier NATIVE = new TypeSpecifier();
        public static final TypeSpecifier SIGNED = new TypeSpecifier();
        public static final TypeSpecifier UNSIGNED = new TypeSpecifier();
        public static final TypeSpecifier INTEGER = new TypeSpecifier();
        public static final TypeSpecifier FLOAT = new TypeSpecifier();
        public static final TypeSpecifier BINARY = new TypeSpecifier();
        public static final TypeSpecifier BITSTRING = new TypeSpecifier();
        public static final TypeSpecifier UTF8 = new TypeSpecifier();
        public static final TypeSpecifier UTF16 = new TypeSpecifier();
        public static final TypeSpecifier UTF32 = new TypeSpecifier();

        public static final int MIN_UNIT_SIZE = 1;
        public static final int MAX_UNIT_SIZE = 256;

        private int unitSize;

        private TypeSpecifier() {
            this.unitSize = 0;
        }

        private TypeSpecifier(int unitSize) {
            this.unitSize = unitSize;
        }

        public static TypeSpecifier unit(final int size) {

            if (MIN_UNIT_SIZE <= size && size <= MAX_UNIT_SIZE) {
                return new TypeSpecifier(size);
            }

            throw ErlControlException.makeBadarg();
        }

        public int getUnitSize() {
            return unitSize;
        }
    }

    private static boolean haveSpecifier(final TypeSpecifier spec, final TypeSpecifier[] specifiers) {

        if (null != specifiers) {

            for (TypeSpecifier s : specifiers) {
                if (spec == s) {
                    return true;
                }
            }
        }

        return false;
    }

    private static int findUnitSize(final TypeSpecifier[] specifiers) {

        if (null != specifiers) {

            for (TypeSpecifier spec : specifiers) {
                if (0 != spec.unitSize) {
                    return spec.unitSize;
                }
            }
        }

        return 0;
    }

    private int evaluateSizeAsInt(VirtualFrame frame) {

        final Object result = sizeExprNode.executeGeneric(frame);

        if (result instanceof Long) {
            final long value = (long) result;

            if (0 <= value && value <= Integer.MAX_VALUE) {
                return (int) value;
            }

        } else if (result instanceof BigInteger) {
            try {
                final int value = ((BigInteger) result).intValueExact();

                if (0 <= value) {
                    return value;
                }

            } catch (ArithmeticException ex) {
                // makeBadarg() will be thrown
            }
        }

        throw ErlControlException.makeBadarg();
    }

    public static final int DEFAULT_INTEGER_SIZE = 8;
    public static final int DEFAULT_DOUBLE_SIZE = 64;
    public static final int DEFAULT_INTEGER_UNIT = 1;
    public static final int DEFAULT_DOUBLE_UNIT = 1;
    public static final int DEFAULT_BITSTRING_UNIT = 1;
    public static final int DEFAULT_BINARY_UNIT = 8;

    @Override
    public ErlBinary executeGeneric(VirtualFrame frame) {

        if (alwaysBadarg) {
            throw ErlControlException.makeBadarg();
        }

        Object value = valueExprNode.executeGeneric(frame);

        if (value instanceof ErlList) {

            Object[] elems = ((ErlList) value).toArray();
            byte[] refByte = new byte[1];
            ErlLazyBinary lazy = null;

            for (int i = 0; i < elems.length; ++i) {
                if (ErlContext.isByte(elems[i], refByte)) {

                    ErlBinary bin = constructElement(frame, (long) refByte[0]);

                    if (null != lazy) {
                        lazy = new ErlLazyBinary(lazy, new ErlLazyBinary(bin));
                    } else {
                        lazy = new ErlLazyBinary(bin);
                    }

                } else {
                    throw ErlControlException.makeBadarg();
                }
            }

            return lazy.construct();

        } else {
            return constructElement(frame, value);
        }
    }

    public ErlBinary constructElement(VirtualFrame frame, Object value_) {

        Object value = value_;
        int size;
        int numBytes;
        byte[] bytes;

        if (value instanceof Long || value instanceof BigInteger) {

            int currentUnitSize;

            if (null != encoder) {

                final long codepoint = ErlContext.decodeLong(value);

                if (0 <= codepoint && codepoint <= Unicode.UTF_MAX_VALUE) {
                    try {
                        encoder.reset();
                        bytes = encoder.encode((int) codepoint);
                        size = bytes.length * ErlBinary.BITS_PER_BYTE;
                        currentUnitSize = size;
                    } catch (EncodeFailedException ex) {
                        throw ErlControlException.makeBadarg();
                    }
                } else {
                    throw ErlControlException.makeBadarg();
                }

            } else {
                if (value instanceof Long) {
                    bytes = ByteBuffer.allocate(Long.SIZE / Byte.SIZE).putLong((long) value).array();
                } else {
                    bytes = ((BigInteger) value).toByteArray();
                }

                if (TypeSpecifier.INTEGER == typeSpec) {
                    currentUnitSize = (0 == unitSize) ? DEFAULT_INTEGER_UNIT : unitSize;
                    size = currentUnitSize * ((null != sizeExprNode) ? evaluateSizeAsInt(frame) : DEFAULT_INTEGER_SIZE);
                } else {
                    throw ErlControlException.makeBadarg();
                }
            }

            numBytes = (size + ErlBinary.BITS_PER_BYTE - 1) / ErlBinary.BITS_PER_BYTE;

            if (bytes.length < numBytes) {

                byte[] tmp = new byte[numBytes];

                int srcIndex = 0;
                int dstIndex = numBytes - bytes.length;
                while (dstIndex < numBytes) {
                    tmp[dstIndex++] = bytes[srcIndex++];
                }

                bytes = tmp;
            }

        } else if (value instanceof Double) {

            if (TypeSpecifier.FLOAT != typeSpec) {
                throw ErlControlException.makeBadarg();
            }

            int currentUnitSize = (0 == unitSize) ? DEFAULT_DOUBLE_UNIT : unitSize;
            size = currentUnitSize * ((null != sizeExprNode) ? evaluateSizeAsInt(frame) : DEFAULT_DOUBLE_SIZE);
            numBytes = (size + ErlBinary.BITS_PER_BYTE - 1) / ErlBinary.BITS_PER_BYTE;

            if (Double.SIZE == size) {
                bytes = ByteBuffer.allocate(Double.SIZE / Byte.SIZE).putDouble((double) value).array();
            } else if (Float.SIZE == size) {
                bytes = ByteBuffer.allocate(Float.SIZE / Byte.SIZE).putFloat((float) value).array();
            } else {
                throw ErlControlException.makeBadarg();
            }

        } else {

            if (value instanceof ErlLazyBinary) {
                value = ((ErlLazyBinary) value).construct();
            } else if (value instanceof ErlBinaryView) {
                value = ((ErlBinaryView) value).construct();
            }

            if (value instanceof ErlBinary) {

                ErlBinary bin = (ErlBinary) value;
                final boolean isBitString = bin.hasByteFragment();
                int currentUnitSize;

                if (TypeSpecifier.BITSTRING == typeSpec) {

                    currentUnitSize = (0 == unitSize) ? DEFAULT_BITSTRING_UNIT : unitSize;
                    size = currentUnitSize * ((null != sizeExprNode) ? evaluateSizeAsInt(frame) : bin.getBitSize());

                } else if (TypeSpecifier.BINARY == typeSpec) {

                    if (isBitString) {
                        throw ErlControlException.makeBadarg();
                    }

                    currentUnitSize = (0 == unitSize) ? DEFAULT_BINARY_UNIT : unitSize;
                    size = currentUnitSize * ((null != sizeExprNode) ? evaluateSizeAsInt(frame) : bin.getByteSize());

                } else {

                    throw ErlControlException.makeBadarg();
                }

                numBytes = (size + ErlBinary.BITS_PER_BYTE - 1) / ErlBinary.BITS_PER_BYTE;

                if (size > bin.getBitSize()) {
                    throw ErlControlException.makeBadarg();
                }

                bytes = bin.extractLeft(0, size);

            } else {

                throw ErlControlException.makeBadarg();
            }
        }

        final boolean rightAligned = (TypeSpecifier.BINARY != typeSpec) && (TypeSpecifier.BITSTRING != typeSpec);

        if (needByteOrderSwap && rightAligned && null == encoder) {
            for (int i = 0; i < bytes.length / 2; i++) {
                final byte tmp = bytes[i];
                bytes[i] = bytes[bytes.length - i - 1];
                bytes[bytes.length - i - 1] = tmp;
            }
        }

        if (bytes.length != numBytes) {
            if (needByteOrderSwap && rightAligned) {
                bytes = Arrays.copyOf(bytes, numBytes);
            } else {
                bytes = Arrays.copyOfRange(bytes, bytes.length - numBytes, bytes.length);
            }
        }

        final int usedBits = size % ErlBinary.BITS_PER_BYTE;
        final int unusedBits = (0 == usedBits) ? 0 : (ErlBinary.BITS_PER_BYTE - usedBits);

        if (rightAligned && 0 != numBytes && 0 != usedBits) {

            // convert left aligned array to right aligned

            bytes[0] <<= unusedBits;

            for (int i = 1; i < numBytes; ++i) {
                bytes[i - 1] |= (byte) (Byte.toUnsignedInt(bytes[i]) >> usedBits);
                bytes[i] <<= unusedBits;
            }
        }

        return ErlBinary.fromArray(unusedBits, bytes);
    }

    private ByteBuffer byteBufferFrom(final byte[] bytes, final int size) {
        return byteBufferFrom(bytes, size, (byte) 0);
    }

    private ByteBuffer byteBufferFrom(final byte[] bytes, final int size, final byte padByte) {

        ByteBuffer buf = ByteBuffer.allocate(size);

        if (!needByteOrderSwap) {
            for (int i = bytes.length; i < size; ++i) {
                buf.put(padByte);
            }
        }

        buf.put(bytes);

        if (needByteOrderSwap) {
            for (int i = bytes.length; i < size; ++i) {
                buf.put(padByte);
            }

            buf.order(ByteOrder.LITTLE_ENDIAN);
        }

        return buf;
    }

    public int match(VirtualFrame frame, ErlBinary bin, int bitOffset, int bitSize) {

        if (alwaysBadarg) {
            throw ErlControlException.makeBadmatch(bin);
        }

        if (null != decoder) {

            return matchDecoded(frame, bin, bitOffset, bitSize);

        } else if (TypeSpecifier.INTEGER == typeSpec) {

            return matchInteger(frame, bin, bitOffset, bitSize);

        } else if (TypeSpecifier.FLOAT == typeSpec) {

            return matchFloat(frame, bin, bitOffset, bitSize);

        } else if (TypeSpecifier.BINARY == typeSpec || TypeSpecifier.BITSTRING == typeSpec) {

            return matchBinary(frame, bin, bitOffset, bitSize, TypeSpecifier.BITSTRING == typeSpec);

        } else {

            throw ErlControlException.makeBadmatch(bin);
        }
    }

    private int matchInteger(VirtualFrame frame, ErlBinary bin, int bitOffset, int bitSize) {

        final int currentUnitSize = (0 == unitSize) ? DEFAULT_INTEGER_UNIT : unitSize;
        final int size = currentUnitSize * ((null != sizeExprNode) ? evaluateSizeAsInt(frame) : DEFAULT_INTEGER_SIZE);

        if (size > bitSize) {
            throw ErlControlException.makeBadmatch(bin);
        }

        byte[] bytes = bin.extract(bitOffset, size, needByteOrderSwap);

        if (bytes.length == 0) {

            valueExprNode.match(frame, (long) 0);
            return size;

        }

        final int usedBits = size & (ErlBinary.BITS_PER_BYTE - 1);
        byte padByte = (byte) 0;

        if (needByteOrderSwap) {

            if (isSigned) {

                if (0 != (bytes[bytes.length - 1] & 0x80)) {
                    padByte = (byte) 255;
                }

                if (0 != usedBits) {
                    // signed shift
                    bytes[bytes.length - 1] >>= (ErlBinary.BITS_PER_BYTE - usedBits);
                }
            } else if (0 != usedBits) {
                // unsigned shift
                final int ub = Byte.toUnsignedInt(bytes[bytes.length - 1]);
                bytes[bytes.length - 1] = (byte) (ub >> (ErlBinary.BITS_PER_BYTE - usedBits));
            }

        } else if (isSigned) {

            if (0 != (bytes[0] & (0x80 >> (ErlBinary.BITS_PER_BYTE - usedBits)))) {

                bytes[0] |= 0xff << usedBits;
                padByte = (byte) 255;
            }

        }

        if (bytes.length <= Long.SIZE / Byte.SIZE) {

            final long number = byteBufferFrom(bytes, Long.SIZE / Byte.SIZE, padByte).getLong(0);

            valueExprNode.match(frame, number);

        } else {

            if (needByteOrderSwap) {
                for (int i = 0; i < bytes.length / 2; i++) {
                    final byte tmp = bytes[i];
                    bytes[i] = bytes[bytes.length - i - 1];
                    bytes[bytes.length - i - 1] = tmp;
                }
            }

            BigInteger number;

            if (isSigned) {
                number = new BigInteger(bytes);
            } else {
                number = new BigInteger(1, bytes);
            }

            valueExprNode.match(frame, number);
        }

        return size;
    }

    private int matchDecoded(VirtualFrame frame, ErlBinary bin, int bitOffset, int bitSize) {

        try {
            decoder.reset();

            final int bitSize2 = bitSize - ErlBinary.BITS_PER_BYTE;
            int size = 0;

            do {
                if (size > bitSize2) {
                    throw ErlControlException.makeBadmatch(bin);
                }

                byte[] bytes = bin.extractLeft(bitOffset + size, ErlBinary.BITS_PER_BYTE);
                decoder.feed(bytes[0]);
                size += ErlBinary.BITS_PER_BYTE;

            } while (!decoder.isComplete());

            valueExprNode.match(frame, (long) decoder.getCodepoint());

            return size;

        } catch (DecodeFailedException ex) {
            throw ErlControlException.makeBadmatch(bin);
        }
    }

    private int matchFloat(VirtualFrame frame, ErlBinary bin, int bitOffset, int bitSize) {

        final int currentUnitSize = (0 == unitSize) ? DEFAULT_DOUBLE_UNIT : unitSize;
        final int size = currentUnitSize * ((null != sizeExprNode) ? evaluateSizeAsInt(frame) : DEFAULT_DOUBLE_SIZE);

        if (size > bitSize) {
            throw ErlControlException.makeBadmatch(bin);
        }

        byte[] bytes = bin.extractRight(bitOffset, size);

        if (bytes.length == 0) {

            valueExprNode.match(frame, (double) 0);
            return size;

        } else if (bytes.length == Double.SIZE / Byte.SIZE) {

            final double number = byteBufferFrom(bytes, Double.SIZE / Byte.SIZE).getDouble(0);

            valueExprNode.match(frame, number);

        } else if (bytes.length == Float.SIZE / Byte.SIZE) {

            final float number = byteBufferFrom(bytes, Float.SIZE / Byte.SIZE).getFloat(0);

            valueExprNode.match(frame, (double) number);

        } else {

            throw ErlControlException.makeBadmatch(bin);
        }

        return size;
    }

    private int matchBinary(VirtualFrame frame, ErlBinary bin, int bitOffset, int bitSize, boolean isBitString) {

        int currentUnitSize;
        int size;

        if (isBitString) {

            currentUnitSize = (0 == unitSize) ? DEFAULT_BITSTRING_UNIT : unitSize;
            size = currentUnitSize * ((null != sizeExprNode) ? evaluateSizeAsInt(frame) : bitSize);

        } else {

            if (0 != (bitSize & (ErlBinary.BITS_PER_BYTE - 1))) {
                throw ErlControlException.makeBadarg();
            }

            currentUnitSize = (0 == unitSize) ? DEFAULT_BINARY_UNIT : unitSize;
            size = currentUnitSize * ((null != sizeExprNode) ? evaluateSizeAsInt(frame) : (bitSize / ErlBinary.BITS_PER_BYTE));
        }

        if (size > bitSize) {
            throw ErlControlException.makeBadmatch(bin);
        }

        final int usedBits = (size & (ErlBinary.BITS_PER_BYTE - 1));
        final int unusedBits = (0 == usedBits) ? 0 : (ErlBinary.BITS_PER_BYTE - usedBits);

        byte[] bytes = bin.extractLeft(bitOffset, size);

        ErlBinary part = ErlBinary.fromArray(unusedBits, bytes);
        valueExprNode.match(frame, part);

        return size;
    }
}
