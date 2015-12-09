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

import java.io.File;
import java.io.FilenameFilter;
import java.nio.ByteBuffer;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.erl.runtime.drivers.efile.CloseAction;
import com.oracle.truffle.erl.runtime.drivers.efile.FstatAction;
import com.oracle.truffle.erl.runtime.drivers.efile.OpenAction;
import com.oracle.truffle.erl.runtime.drivers.efile.PwdAction;
import com.oracle.truffle.erl.runtime.drivers.efile.ReadAction;
import com.oracle.truffle.erl.runtime.drivers.efile.ReadDirAction;
import com.oracle.truffle.erl.runtime.drivers.efile.ReadFileAction;
import com.oracle.truffle.erl.runtime.drivers.efile.SeekAction;
import com.oracle.truffle.erl.runtime.drivers.efile.WriteAction;
import com.oracle.truffle.erl.runtime.misc.PortOptions;

public final class Efile extends Driver {

    private FDFile openedFile = null;

    public static String getDriverName() {
        return "efile";
    }

    private Efile() {
        super(getDriverName());
    }

    @Override
    protected void closeDriver() {
        if (null != openedFile) {
            openedFile.close();
        }
    }

    public static Efile create(@SuppressWarnings("unused") String command, PortOptions po) {
        if (po.isBinary()) {
            return new Efile();
        }

        return null;
    }

    public static final FilenameFilter READDIR_FILTER = new FilenameFilter() {

        @Override
        @TruffleBoundary
        public boolean accept(File dir, String name) {
            return !".".equals(name) && !"..".equals(name);
        }
    };

    /**
     * The efile has a quite strange behavior when opening files. Only one file can be opened at the
     * same time. In order to protect ourself from keeping garbage (file descriptors that are
     * unaccessible from Erlang), we simply close the previous file.
     */
    @TruffleBoundary
    public void registerOpenedFile(FDFile file) {

        if (null != openedFile) {
            openedFile.close();
        }

        openedFile = file;
    }

    @Override
    @TruffleBoundary
    protected AsyncAction parseCommand(byte[] data) {

        switch (data[0]) {

            case FILE_OPEN: {
                final String name = (data.length >= 6 && 0 == data[data.length - 1]) ? (new String(data, 5, data.length - 6)) : null;
                final int mode = extractIntMSB(data, 1);
                return new OpenAction(this, name, mode);
            }

            case FILE_READ: {
                final long size = extractLongMSB(data, 1);
                return new ReadAction(openedFile, size);
            }

            case FILE_LSEEK: {
                final long offset = extractLongMSB(data, 1);
                final int whence = extractIntMSB(data, 1 + 8);
                return new SeekAction(openedFile, offset, whence);
            }

            case FILE_WRITE: {
                final ByteBuffer buf = ByteBuffer.wrap(data, 1, data.length - 1);
                return new WriteAction(openedFile, buf);
            }

            case FILE_FSTAT:
            case FILE_LSTAT: {
                final String name = (data.length >= 3 && 0 == data[data.length - 1]) ? (new String(data, 1, data.length - 2)) : null;
                return new FstatAction(name, FILE_LSTAT == data[0]);
            }

            case FILE_PWD: {
                return new PwdAction();
            }

            case FILE_READ_FILE: {
                final String name = (data.length >= 3 && 0 == data[data.length - 1]) ? (new String(data, 1, data.length - 2)) : null;
                return new ReadFileAction(name);
            }

            case FILE_READDIR: {
                final String name = (data.length >= 3 && 0 == data[data.length - 1]) ? (new String(data, 1, data.length - 2)) : null;
                return new ReadDirAction(name);
            }

            case FILE_CLOSE: {
                final FDFile file = openedFile;
                openedFile = null;
                return new CloseAction(file);
            }
        }

        System.err.println("efile: command " + Byte.toUnsignedInt(data[0]) + " is not implemented");
        return null;
    }
}
