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

// This class computes MD5 hashes.
// Manually translated by Jon Howell <jonh@cs.dartmouth.edu>
// from some public domain C code (md5.c) included with the ssh-1.2.22 source.
// Tue Jan 19 15:55:50 EST 1999
// $Id: MD5.java,v 1.3 1999/01/19 21:30:11 jonh Exp jonh $
//
// To compute the message digest of a chunk of bytes, create an
// MD5 object 'md5', call md5.update() as needed on buffers full
// of bytes, and then call md5.md5final(), which
// will fill a supplied 16-byte array with the digest.
//
// It seems to run around 25-30 times slower (JDK1.1.6) than optimized C
// (gcc -O4, version 2.7.2.3). Measured on a Sun Ultra 5 (SPARC 270MHz).
//
// Comments from md5.c from ssh-1.2.22, the basis for this code:
//
/* This code has been heavily hacked by Tatu Ylonen <ylo@cs.hut.fi> to
   make it compile on machines like Cray that don't have a 32 bit integer
   type. */
/*
 * This code implements the MD5 message-digest algorithm.
 * The algorithm is due to Ron Rivest.  This code was
 * written by Colin Plumb in 1993, no copyright is claimed.
 * This code is in the public domain; do with it what you wish.
 *
 * Equivalent code is available from RSA Data Security, Inc.
 * This code has been tested against that, and is equivalent,
 * except that you don't need to include two pages of legalese
 * with every copy.
 *
 * To compute the message digest of a chunk of bytes, declare an
 * MD5Context structure, pass it to MD5Init, call MD5Update as
 * needed on buffers full of bytes, and then call MD5Final, which
 * will fill a supplied 16-byte array with the digest.
 */

package com.oracle.truffle.erl.runtime.misc;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;

import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;

/**
 * MD5 helper.
 */
public final class MD5 {

    public MD5() {
        reset();
    }

    public MD5(byte[] bin) {
        restore(bin);
    }

    public final static class MD5Visitor extends IODataVisitor {

        private MD5 md5;

        public MD5Visitor() {
            super();
            this.md5 = new MD5();
        }

        public MD5Visitor(MD5 md5) {
            super();
            this.md5 = md5;
        }

        @Override
        protected void visit(byte b) {
            md5.update(new byte[]{b});
        }

        @Override
        protected void visit(final byte[] bs) {
            md5.update(bs);
        }

        public byte[] getDigest() {
            return md5.digest();
        }
    }

    private static final int BLOCK_SIZE = 64;
    private static final int BUFFER_WORDS = 4;
    private static final int BINARY_SIZE = Integer.BYTES * BUFFER_WORDS + BLOCK_SIZE + Long.BYTES;

    int buf[] = new int[BUFFER_WORDS];  // These were originally unsigned ints.
    // This Java code makes an effort to avoid sign traps.
    // buf[] is where the hash accumulates.
    long bits = 0;  // This is the count of bits hashed so far.
    byte in[] = new byte[BLOCK_SIZE];  // This is a buffer where we stash bytes until we have
    // enough (64) to perform a transform operation.
    int inint[] = new int[16];
    // inint[] used and discarded inside transform(),
    // but why allocate it over and over?
    // (In the C version this is allocated on the stack.)

    public static final byte initialState[] = {0x01, 0x23, 0x45, 0x67, (byte) 0x89, (byte) 0xab, (byte) 0xcd, (byte) 0xef, (byte) 0xfe, (byte) 0xdc, (byte) 0xba, (byte) 0x98, 0x76, 0x54, 0x32, 0x10,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

    private void reset() {

        buf[0] = 0x67452301;
        buf[1] = 0xefcdab89;
        buf[2] = 0x98badcfe;
        buf[3] = 0x10325476;

        bits = 0;
        Arrays.fill(in, (byte) 0);
    }

    public void restore(byte[] bin) {

        if (bin.length != BINARY_SIZE) {
            throw ErlControlException.makeBadarg();
        }

        ByteBuffer bb = ByteBuffer.allocate(BINARY_SIZE);
        bb.order(ByteOrder.nativeOrder());
        bb.put(bin);
        bb.position(0);

        for (int i = 0; i < buf.length; ++i) {
            buf[i] = bb.getInt();
        }

        bb.get(in);

        bits = bb.getLong();
    }

    public byte[] serialize() {

        ByteBuffer bb = ByteBuffer.allocate(BINARY_SIZE);
        bb.order(ByteOrder.nativeOrder());

        for (int i = 0; i < buf.length; ++i) {
            bb.putInt(buf[i]);
        }

        bb.put(in);
        bb.putLong(bits);

        return bb.array();
    }

    public void update(byte[] newbuf) {
        update(newbuf, 0, newbuf.length);
    }

    public void update(byte[] newbuf, int length) {
        update(newbuf, 0, length);
    }

    public void update(byte[] newbuf, int bufstart_, int buflen) {
        int t;
        int len = buflen;
        int bufstart = bufstart_;

        // shash old bits value for the "Bytes already in" computation
        // just below.
        t = (int) bits; // (int) cast should just drop high bits, I hope

        /* update bitcount */
        /*
         * the C code used two 32-bit ints separately, and carefully ensured that the carry carried.
         * Java has a 64-bit long, which is just what the code really wants.
         */
        bits += (len << 3);

        t = (t >>> 3) & 0x3f; /* Bytes already in this->in */

        /* Handle any leading odd-sized chunks */
        /* (that is, any left-over chunk left by last update() */

        if (t != 0) {
            int p = t;
            t = 64 - t;
            if (len < t) {
                System.arraycopy(newbuf, bufstart, in, p, len);
                return;
            }
            System.arraycopy(newbuf, bufstart, in, p, t);
            transform();
            bufstart += t;
            len -= t;
        }

        /* Process data in 64-byte chunks */
        while (len >= 64) {
            System.arraycopy(newbuf, bufstart, in, 0, 64);
            transform();
            bufstart += 64;
            len -= 64;
        }

        /* Handle any remaining bytes of data. */
        /* that is, stash them for the next update(). */
        System.arraycopy(newbuf, bufstart, in, 0, len);
    }

    /*
     * Final wrapup - pad to 64-byte boundary with the bit pattern 1 0* (64-bit count of bits
     * processed, MSB-first)
     */
    public void md5final(byte[] digest) {
        /* "final" is a poor method name in Java. :v) */
        int count;
        int p;      // in original code, this is a pointer; in this java code
                    // it's an index into the array this->in.

        /* Compute number of bytes mod 64 */
        count = (int) ((bits >>> 3) & 0x3F);

        /*
         * Set the first char of padding to 0x80. This is safe since there is always at least one
         * byte free
         */
        p = count;
        in[p++] = (byte) 0x80;

        /* Bytes of padding needed to make 64 bytes */
        count = 64 - 1 - count;

        /* Pad out to 56 mod 64 */
        if (count < 8) {
            /* Two lots of padding: Pad the first block to 64 bytes */
            zeroByteArray(in, p, count);
            transform();

            /* Now fill the next block with 56 bytes */
            zeroByteArray(in, 0, 56);
        } else {
            /* Pad block to 56 bytes */
            zeroByteArray(in, p, count - 8);
        }

        /* Append length in bits and transform */
        // Could use a PUT_64BIT... func here. This is a fairly
        // direct translation from the C code, where bits was an array
        // of two 32-bit ints.
        int lowbits = (int) bits;
        int highbits = (int) (bits >>> 32);
        PUT_32BIT_LSB_FIRST(in, 56, lowbits);
        PUT_32BIT_LSB_FIRST(in, 60, highbits);

        transform();
        PUT_32BIT_LSB_FIRST(digest, 0, buf[0]);
        PUT_32BIT_LSB_FIRST(digest, 4, buf[1]);
        PUT_32BIT_LSB_FIRST(digest, 8, buf[2]);
        PUT_32BIT_LSB_FIRST(digest, 12, buf[3]);

        /* zero sensitive data */
        /*
         * notice this misses any sneaking out on the stack. The C version uses registers in some
         * spots, perhaps because they care about this.
         */
        zeroByteArray(in);
        zeroIntArray(buf);
        bits = 0;
        zeroIntArray(inint);
    }

    public byte[] digest() {

        byte[] d = new byte[16];
        md5final(d);
        return d;
    }

    /////////////////////////////////////////////////////////////////////
    // Below here ye will only find private functions //
    /////////////////////////////////////////////////////////////////////

    // There must be a way to do these functions that's
    // built into Java, and I just haven't noticed it yet.

    private static void zeroByteArray(byte[] a) {
        zeroByteArray(a, 0, a.length);
    }

    private static void zeroByteArray(byte[] a, int start, int length) {
        setByteArray(a, (byte) 0, start, length);
    }

    private static void setByteArray(byte[] a, byte val, int start, int length) {
        int i;
        int end = start + length;
        for (i = start; i < end; i++) {
            a[i] = val;
        }
    }

    private static void zeroIntArray(int[] a) {
        zeroIntArray(a, 0, a.length);
    }

    private static void zeroIntArray(int[] a, int start, int length) {
        setIntArray(a, 0, start, length);
    }

    private static void setIntArray(int[] a, int val, int start, int length) {
        int i;
        int end = start + length;
        for (i = start; i < end; i++) {
            a[i] = val;
        }
    }

    // In the C version, a call to MD5STEP is a macro-in-a-macro.
    // In this Java version, we pass an Fcore object to represent the
    // inner macro, and the MD5STEP() method performs the work of
    // the outer macro. It would be good if this could all get
    // inlined, but it would take a pretty aggressive compiler to
    // inline away the dynamic method lookup made by MD5STEP to
    // get to the Fcore.f function.

    private abstract class Fcore {
        abstract int f(int x, int y, int z);
    }

    private Fcore F1 = new Fcore() {
        @Override
        int f(int x, int y, int z) {
            return (z ^ (x & (y ^ z)));
        }
    };
    private Fcore F2 = new Fcore() {
        @Override
        int f(int x, int y, int z) {
            return (y ^ (z & (x ^ y)));
        }
    };
    private Fcore F3 = new Fcore() {
        @Override
        int f(int x, int y, int z) {
            return (x ^ y ^ z);
        }
    };
    private Fcore F4 = new Fcore() {
        @Override
        int f(int x, int y, int z) {
            return (y ^ (x | ~z));
        }
    };

    private static int MD5STEP(Fcore f, int w_, int x, int y, int z, int data, int s) {
        int w = w_;
        w += f.f(x, y, z) + data;
        w = w << s | w >>> (32 - s);
        w += x;
        return w;
    }

    private void transform() {
        /* load in[] byte array into an internal int array */
        int i;

        for (i = 0; i < 16; i++) {
            inint[i] = GET_32BIT_LSB_FIRST(in, 4 * i);
        }

        int a, b, c, d;
        a = buf[0];
        b = buf[1];
        c = buf[2];
        d = buf[3];

        a = MD5STEP(F1, a, b, c, d, inint[0] + 0xd76aa478, 7);
        d = MD5STEP(F1, d, a, b, c, inint[1] + 0xe8c7b756, 12);
        c = MD5STEP(F1, c, d, a, b, inint[2] + 0x242070db, 17);
        b = MD5STEP(F1, b, c, d, a, inint[3] + 0xc1bdceee, 22);
        a = MD5STEP(F1, a, b, c, d, inint[4] + 0xf57c0faf, 7);
        d = MD5STEP(F1, d, a, b, c, inint[5] + 0x4787c62a, 12);
        c = MD5STEP(F1, c, d, a, b, inint[6] + 0xa8304613, 17);
        b = MD5STEP(F1, b, c, d, a, inint[7] + 0xfd469501, 22);
        a = MD5STEP(F1, a, b, c, d, inint[8] + 0x698098d8, 7);
        d = MD5STEP(F1, d, a, b, c, inint[9] + 0x8b44f7af, 12);
        c = MD5STEP(F1, c, d, a, b, inint[10] + 0xffff5bb1, 17);
        b = MD5STEP(F1, b, c, d, a, inint[11] + 0x895cd7be, 22);
        a = MD5STEP(F1, a, b, c, d, inint[12] + 0x6b901122, 7);
        d = MD5STEP(F1, d, a, b, c, inint[13] + 0xfd987193, 12);
        c = MD5STEP(F1, c, d, a, b, inint[14] + 0xa679438e, 17);
        b = MD5STEP(F1, b, c, d, a, inint[15] + 0x49b40821, 22);

        a = MD5STEP(F2, a, b, c, d, inint[1] + 0xf61e2562, 5);
        d = MD5STEP(F2, d, a, b, c, inint[6] + 0xc040b340, 9);
        c = MD5STEP(F2, c, d, a, b, inint[11] + 0x265e5a51, 14);
        b = MD5STEP(F2, b, c, d, a, inint[0] + 0xe9b6c7aa, 20);
        a = MD5STEP(F2, a, b, c, d, inint[5] + 0xd62f105d, 5);
        d = MD5STEP(F2, d, a, b, c, inint[10] + 0x02441453, 9);
        c = MD5STEP(F2, c, d, a, b, inint[15] + 0xd8a1e681, 14);
        b = MD5STEP(F2, b, c, d, a, inint[4] + 0xe7d3fbc8, 20);
        a = MD5STEP(F2, a, b, c, d, inint[9] + 0x21e1cde6, 5);
        d = MD5STEP(F2, d, a, b, c, inint[14] + 0xc33707d6, 9);
        c = MD5STEP(F2, c, d, a, b, inint[3] + 0xf4d50d87, 14);
        b = MD5STEP(F2, b, c, d, a, inint[8] + 0x455a14ed, 20);
        a = MD5STEP(F2, a, b, c, d, inint[13] + 0xa9e3e905, 5);
        d = MD5STEP(F2, d, a, b, c, inint[2] + 0xfcefa3f8, 9);
        c = MD5STEP(F2, c, d, a, b, inint[7] + 0x676f02d9, 14);
        b = MD5STEP(F2, b, c, d, a, inint[12] + 0x8d2a4c8a, 20);

        a = MD5STEP(F3, a, b, c, d, inint[5] + 0xfffa3942, 4);
        d = MD5STEP(F3, d, a, b, c, inint[8] + 0x8771f681, 11);
        c = MD5STEP(F3, c, d, a, b, inint[11] + 0x6d9d6122, 16);
        b = MD5STEP(F3, b, c, d, a, inint[14] + 0xfde5380c, 23);
        a = MD5STEP(F3, a, b, c, d, inint[1] + 0xa4beea44, 4);
        d = MD5STEP(F3, d, a, b, c, inint[4] + 0x4bdecfa9, 11);
        c = MD5STEP(F3, c, d, a, b, inint[7] + 0xf6bb4b60, 16);
        b = MD5STEP(F3, b, c, d, a, inint[10] + 0xbebfbc70, 23);
        a = MD5STEP(F3, a, b, c, d, inint[13] + 0x289b7ec6, 4);
        d = MD5STEP(F3, d, a, b, c, inint[0] + 0xeaa127fa, 11);
        c = MD5STEP(F3, c, d, a, b, inint[3] + 0xd4ef3085, 16);
        b = MD5STEP(F3, b, c, d, a, inint[6] + 0x04881d05, 23);
        a = MD5STEP(F3, a, b, c, d, inint[9] + 0xd9d4d039, 4);
        d = MD5STEP(F3, d, a, b, c, inint[12] + 0xe6db99e5, 11);
        c = MD5STEP(F3, c, d, a, b, inint[15] + 0x1fa27cf8, 16);
        b = MD5STEP(F3, b, c, d, a, inint[2] + 0xc4ac5665, 23);

        a = MD5STEP(F4, a, b, c, d, inint[0] + 0xf4292244, 6);
        d = MD5STEP(F4, d, a, b, c, inint[7] + 0x432aff97, 10);
        c = MD5STEP(F4, c, d, a, b, inint[14] + 0xab9423a7, 15);
        b = MD5STEP(F4, b, c, d, a, inint[5] + 0xfc93a039, 21);
        a = MD5STEP(F4, a, b, c, d, inint[12] + 0x655b59c3, 6);
        d = MD5STEP(F4, d, a, b, c, inint[3] + 0x8f0ccc92, 10);
        c = MD5STEP(F4, c, d, a, b, inint[10] + 0xffeff47d, 15);
        b = MD5STEP(F4, b, c, d, a, inint[1] + 0x85845dd1, 21);
        a = MD5STEP(F4, a, b, c, d, inint[8] + 0x6fa87e4f, 6);
        d = MD5STEP(F4, d, a, b, c, inint[15] + 0xfe2ce6e0, 10);
        c = MD5STEP(F4, c, d, a, b, inint[6] + 0xa3014314, 15);
        b = MD5STEP(F4, b, c, d, a, inint[13] + 0x4e0811a1, 21);
        a = MD5STEP(F4, a, b, c, d, inint[4] + 0xf7537e82, 6);
        d = MD5STEP(F4, d, a, b, c, inint[11] + 0xbd3af235, 10);
        c = MD5STEP(F4, c, d, a, b, inint[2] + 0x2ad7d2bb, 15);
        b = MD5STEP(F4, b, c, d, a, inint[9] + 0xeb86d391, 21);

        buf[0] += a;
        buf[1] += b;
        buf[2] += c;
        buf[3] += d;
    }

    private static int GET_32BIT_LSB_FIRST(byte[] b, int off) {
        return (b[off + 0] & 0xff) | ((b[off + 1] & 0xff) << 8) | ((b[off + 2] & 0xff) << 16) | ((b[off + 3] & 0xff) << 24);
    }

    private static void PUT_32BIT_LSB_FIRST(byte[] b, int off, int value) {
        b[off + 0] = (byte) (value & 0xff);
        b[off + 1] = (byte) ((value >> 8) & 0xff);
        b[off + 2] = (byte) ((value >> 16) & 0xff);
        b[off + 3] = (byte) ((value >> 24) & 0xff);
    }

}
