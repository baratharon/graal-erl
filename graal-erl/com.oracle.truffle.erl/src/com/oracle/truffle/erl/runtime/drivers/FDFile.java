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
package com.oracle.truffle.erl.runtime.drivers;

import java.io.IOException;
import java.nio.channels.FileChannel;
import java.util.HashMap;
import java.util.Set;

public final class FDFile {

    private static final HashMap<Integer, FDFile> files = new HashMap<>();
    private static int firstFD = 10;
    private static int maxOpenFiles = 100;

    public static final class TooManyFilesException extends RuntimeException {

        private static final long serialVersionUID = 4039521143451074409L;

        private TooManyFilesException() {
        }

        public static final TooManyFilesException INSTANCE = new TooManyFilesException();
    }

    private static synchronized int register(FDFile file) {

        final Set<Integer> fds = files.keySet();

        if (fds.size() >= maxOpenFiles) {
            throw TooManyFilesException.INSTANCE;
        }

        int fd = firstFD;

        // better to reuse fds, since a file can be opened and closed many times

        while (fd > 0) {

            if (!fds.contains(fd)) {
                files.put(fd, file);
                return fd;
            }

            ++fd;
        }

        // If 'fd' wraps, that means too many FDs are present. However, 2 billion opened files are a
        // quite large amount, so we do not expect that many opened files anyway.
        throw TooManyFilesException.INSTANCE;
    }

    private static synchronized void unregister(FDFile file) {
        files.remove(file.getFD());
    }

    private final int fd;
    private final boolean canRead;
    private final boolean canWrite;
    private FileChannel channel;

    public FDFile(FileChannel channel, boolean canRead, boolean canWrite) {
        this.fd = register(this);
        this.canRead = canRead;
        this.canWrite = canWrite;
        this.channel = channel;
    }

    public static synchronized FDFile lookup(int fd) {
        return files.get(fd);
    }

    public int getFD() {
        return fd;
    }

    public void close() {
        if (channel != null) {
            try {
                channel.close();
            } catch (IOException e) {
            }

            channel = null;
            unregister(this);
        }
    }

}
