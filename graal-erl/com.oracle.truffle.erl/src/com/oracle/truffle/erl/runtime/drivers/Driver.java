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

import java.util.ArrayList;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.ErlPid;
import com.oracle.truffle.erl.runtime.ErlPort;
import com.oracle.truffle.erl.runtime.ErlProcess;
import com.oracle.truffle.erl.runtime.ErlTuple;
import com.oracle.truffle.erl.runtime.misc.PortOptions;

public abstract class Driver extends ErlPort {

    private final ArrayList<AsyncAction> actions = new ArrayList<>();
    private boolean isOpen = true;

    public static Driver open(String command, PortOptions po) {

        String[] argv = command.split(" \t", 2);
        if (0 == argv.length) {
            return null;
        }

        final String name = argv[0];

        if (name.equals(Efile.getDriverName())) {
            return Efile.create(command, po);
        }

        if (name.equals(UdpInet.getDriverName())) {
            return UdpInet.create(command, po);
        }

        return null;
    }

    protected Driver(String name) {
        super(name);
    }

    @Override
    public void closeUnderlying() {
        isOpen = false;
    }

    @Override
    public boolean isOpen() {
        return isOpen;
    }

    @Override
    @TruffleBoundary
    protected void step() {
        if (!actions.isEmpty()) {
            AsyncAction action = actions.remove(actions.size() - 1);
            perform(action, false);
        }

        if (!isOpen && actions.isEmpty()) {
            closeDriver();
            onClosed();
        }
    }

    @Override
    public void flush() {
        while (!actions.isEmpty()) {
            AsyncAction action = actions.remove(actions.size() - 1);
            perform(action, true);
        }

        if (!isOpen && actions.isEmpty()) {
            closeDriver();
            onClosed();
        }
    }

    @Override
    public boolean command(final ErlPid sender, byte[] data, boolean nosuspend) {

        if (!isOpen) {
            return false;
        }

        AsyncAction action = parseCommand(data);

        if (null == action) {
            return false;
        }

        action.setPid(sender);
        return perform(action, nosuspend || true); // TODO
    }

    @Override
    @TruffleBoundary
    public Object control(int operation, byte[] data) {

        if (!isOpen) {
            return null;
        }

        return doControl(operation, data);
    }

    @TruffleBoundary
    private boolean perform(AsyncAction action, boolean nosuspend) {

        AsyncAction.Result result = action.async();

        while (nosuspend && AsyncAction.Result.AGAIN == result) {
            result = action.async();
        }

        if (AsyncAction.Result.DONE == result) {
            reply(action);
            return true;
        } else if (AsyncAction.Result.ERROR == result) {
            reply(action);
            return false;
        }

        actions.add(action);
        return true;
    }

    private void reply(AsyncAction action) {

        while (action.hasMore()) {
            final ErlTuple msg = new ErlTuple(this, new ErlTuple(ErlAtom.DATA, action.result()));
            ErlProcess.getCurrentProcess().send(action.getPid(), msg, false, false);
        }
    }

    protected abstract AsyncAction parseCommand(byte[] data);

    protected Object doControl(@SuppressWarnings("unused") int operation, @SuppressWarnings("unused") byte[] data) {
        return null;
    }

    protected abstract void closeDriver();

    @ExplodeLoop
    public static ErlList makeNumberResponse(long number) {

        ErlList list = ErlList.NIL;
        long num = number;

        for (int i = 0; i < 8; ++i) {
            list = new ErlList((num & 0xff), list);
            num >>>= 8;
        }

        return new ErlList((long) FILE_RESP_NUMBER, list);
    }

    @ExplodeLoop
    protected static int extractIntMSB(final byte[] data, final int offset) {

        int acc = Byte.toUnsignedInt(data[offset]);

        for (int i = 1; i < 4; ++i) {
            acc <<= 8;
            acc |= Byte.toUnsignedInt(data[offset + i]);
        }

        return acc;
    }

    @ExplodeLoop
    protected static long extractLongMSB(final byte[] data, final int offset) {

        long acc = Byte.toUnsignedLong(data[offset]);

        for (int i = 1; i < 8; ++i) {
            acc <<= 8;
            acc |= Byte.toUnsignedLong(data[offset + i]);
        }

        return acc;
    }

    @SuppressWarnings("deprecation") public static final ErlList EINVAL = ErlList.fromString("einval");
    @SuppressWarnings("deprecation") public static final ErlList ENOENT = ErlList.fromString("enoent");
    @SuppressWarnings("deprecation") public static final ErlList ENOMEM = ErlList.fromString("enomem");
    @SuppressWarnings("deprecation") public static final ErlList ENOTDIR = ErlList.fromString("enotdir");
    @SuppressWarnings("deprecation") public static final ErlList EISDIR = ErlList.fromString("eisdir");
    @SuppressWarnings("deprecation") public static final ErlList EPERM = ErlList.fromString("eperm");
    @SuppressWarnings("deprecation") public static final ErlList EMFILE = ErlList.fromString("emfile");
    @SuppressWarnings("deprecation") public static final ErlList EXBADSEQ = ErlList.fromString("exbadseq");

    public static final byte FILE_OPEN = 1; // required for boot
    public static final byte FILE_READ = 2;
    public static final byte FILE_LSEEK = 3;
    public static final byte FILE_WRITE = 4;
    public static final byte FILE_FSTAT = 5; // required for boot
    public static final byte FILE_PWD = 6; // required for boot
    public static final byte FILE_READDIR = 7; // required for boot
    public static final byte FILE_READ_FILE = 15; // required for boot
    public static final byte FILE_LSTAT = 19; // required for boot
    public static final byte FILE_CLOSE = 23;

    public static final byte FILE_RESP_OK = 0;
    public static final byte FILE_RESP_ERROR = 1;
    public static final byte FILE_RESP_DATA = 2;
    public static final byte FILE_RESP_NUMBER = 3;
    public static final byte FILE_RESP_INFO = 4;
    public static final byte FILE_RESP_NUMERR = 5;
    public static final byte FILE_RESP_LDATA = 6;
    public static final byte FILE_RESP_N2DATA = 7;
    public static final byte FILE_RESP_EOF = 8;
    public static final byte FILE_RESP_FNAME = 9;
    public static final byte FILE_RESP_ALL_DATA = 10;
    public static final byte FILE_RESP_LFNAME = 11;

    public static final int EFILE_MODE_READ = 1;
    public static final int EFILE_MODE_WRITE = 2;
    public static final int EFILE_MODE_READ_WRITE = 3;
    public static final int EFILE_MODE_APPEND = 4;
    public static final int EFILE_COMPRESSED = 8;
    public static final int EFILE_MODE_EXCL = 16;
    public static final int EFILE_MODE_SYNC = 64;
    public static final int EFILE_MODE_MASK = 127;

    public static final int EFILE_SEEK_SET = 0;
    public static final int EFILE_SEEK_CUR = 1;
    public static final int EFILE_SEEK_END = 2;

    public static final int FT_DEVICE = 1;
    public static final int FT_DIRECTORY = 2;
    public static final int FT_REGULAR = 3;
    public static final int FT_SYMLINK = 4;
    public static final int FT_OTHER = 5;

    public static final byte INET_AF_INET = 1;
    public static final byte INET_AF_INET6 = 2;
    public static final byte INET_AF_ANY = 3;
    public static final byte INET_AF_LOOPBACK = 4;

    public static final byte INET_TYPE_STREAM = 1;
    public static final byte INET_TYPE_DGRAM = 2;
    public static final byte INET_TYPE_SEQPACKET = 3;

    public static final byte INET_MODE_LIST = 1;
    public static final byte INET_MODE_BINARY = 2;

    public static final byte INET_REP_ERROR = 0;
    public static final byte INET_REP_OK = 1;
    public static final byte INET_REP = 2;

    public static final byte INET_OPT_LINGER = 3;

    public static final int INET_F_OPEN = 0x1;
    public static final int INET_F_BOUND = 0x2;
    public static final int INET_F_ACTIVE = 0x4;
    public static final int INET_F_LISTEN = 0x8;
    public static final int INET_F_CON = 0x10;
    public static final int INET_F_ACC = 0x20;
    public static final int INET_F_LST = 0x40;
    public static final int INET_F_BUSY = 0x80;

    public static final byte INET_SUBS_EMPTY_OUT_Q = 1;

    public static final int INET_REQ_OPEN = 1;
    public static final int INET_REQ_SETOPTS = 7;
    public static final int INET_REQ_GETOPTS = 8;
    public static final int INET_REQ_SUBSCRIBE = 24;

    public static final int CTRL_OP_GET_WINSIZE = 100;
}
