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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;

public abstract class UnifiedInput {

    public abstract void close();

    public abstract boolean eof();

    public abstract int read() throws IOException;

    public abstract int read(char[] buf) throws IOException;

    private static class InputStreamWrap extends UnifiedInput {
        final InputStream stream;
        private byte[] temp = new byte[32];

        public InputStreamWrap(InputStream stream) {
            this.stream = stream;
        }

        @Override
        public int read() throws IOException {
            return stream.read();
        }

        @Override
        public int read(char[] buf) throws IOException {
            if (temp.length < buf.length) {
                temp = new byte[buf.length];
            }
            final int num = stream.read(temp, 0, buf.length);
            for (int i = 0; i < num; ++i) {
                buf[i] = (char) temp[i];
            }
            return num;
        }

        @Override
        public void close() {
            try {
                stream.close();
            } catch (IOException e) {
            }
        }

        @Override
        public boolean eof() {
            return false;
        }
    }

    private static class BufferedReaderWrap extends UnifiedInput {
        final BufferedReader reader;

        public BufferedReaderWrap(BufferedReader reader) {
            this.reader = reader;
        }

        @Override
        public int read() throws IOException {
            return reader.read();
        }

        @Override
        public int read(char[] buf) throws IOException {
            return reader.read(buf, 0, buf.length);
        }

        @Override
        public void close() {
            try {
                reader.close();
            } catch (IOException e) {
            }
        }

        @Override
        public boolean eof() {
            return false;
        }
    }

    public static UnifiedInput wrap(InputStream stream) {
        return new InputStreamWrap(stream);
    }

    public static UnifiedInput wrap(BufferedReader reader) {
        return new BufferedReaderWrap(reader);
    }
}
