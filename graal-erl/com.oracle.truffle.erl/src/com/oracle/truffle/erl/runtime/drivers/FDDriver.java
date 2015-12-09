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
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlBinary;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.ErlPid;
import com.oracle.truffle.erl.runtime.ErlPort;
import com.oracle.truffle.erl.runtime.ErlProcess;
import com.oracle.truffle.erl.runtime.ErlTuple;
import com.oracle.truffle.erl.runtime.misc.PortOptions;
import com.oracle.truffle.erl.runtime.misc.UnifiedInput;
import com.oracle.truffle.erl.runtime.misc.UnifiedOutput;

public final class FDDriver extends ErlPort {

    private UnifiedInput in;
    private UnifiedOutput out;
    private final boolean eof;
    private ExecutorService executor = null;
    private Future<?> future = null;

    private final class InputReaderThread implements Runnable {

        final FDDriver driver;
        final ErlProcess process;
        final ErlPid pid;

        public InputReaderThread(FDDriver driver, ErlProcess process, ErlPid pid) {
            this.driver = driver;
            this.process = process;
            this.pid = pid;
        }

        @Override
        public void run() {
            char buf[] = new char[32];
            while (!in.eof()) {
                try {
                    int num = in.read(buf);

                    final ErlTuple inner = new ErlTuple(ErlAtom.DATA, ErlBinary.fromChars(buf, num, ErlContext.UTF8_CHARSET));
                    final ErlTuple msg = new ErlTuple(driver, inner);

                    process.send(pid, msg, false, true);

                } catch (InterruptedException ex) {
                    return;
                } catch (IOException ex) {
                    // TODO: EOF
                    return;
                }
            }
        }
    }

    @TruffleBoundary
    private FDDriver(UnifiedInput in, UnifiedOutput out, boolean eof) {
        super("fd");
        this.in = in;
        this.out = out;
        this.eof = eof;

        if (null != in) {
            executor = Executors.newFixedThreadPool(1);
            InputReaderThread thread = new InputReaderThread(this, ErlProcess.getCurrentProcess(), ErlProcess.getSelfPid());
            future = executor.submit(thread);
        }
    }

    @TruffleBoundary
    public static FDDriver create(int fdIn, int fdOut, PortOptions po) {

        UnifiedInput in = null;
        UnifiedOutput out = null;

        if (po.isBinary()) {
            if (1 == fdOut) {
                out = UnifiedOutput.wrap(ErlProcess.getContext().getOutput());
            } else if (2 == fdOut) {
                out = UnifiedOutput.wrap(System.err);
            }

            if (0 == fdIn) {
                in = UnifiedInput.wrap(ErlProcess.getContext().getInput());
            }
        }

        if (po.isOut()) {
            in = null;
        }

        if (po.isIn()) {
            out = null;
        }

        if (null != in || null != out) {
            return new FDDriver(in, out, po.isEof());
        }

        return null;
    }

    @Override
    protected void step() {
        // empty for now
    }

    @Override
    public void flush() {
        // empty for now
    }

    @Override
    @TruffleBoundary
    protected void closeUnderlying() {

        if (null != future) {
            future.cancel(true);
            future = null;
            executor.shutdown();
            executor = null;
        }

        if (null != in) {
            in.close();
            in = null;
        }

        if (null != out) {
            out.flush();
            out.close();
            out = null;
        }
    }

    @Override
    public boolean isOpen() {
        return null != in || null != out;
    }

    @Override
    @TruffleBoundary
    public boolean command(ErlPid sender, byte[] data, boolean nosuspend) {

        if (null == out) {
            return false;
        }

        out.write(data);

        return true;
    }

    @Override
    @TruffleBoundary
    public Object control(int operation, byte[] data) {
        if (Driver.CTRL_OP_GET_WINSIZE == operation) {

            ByteBuffer bb = ByteBuffer.allocate(8);
            bb.order(ByteOrder.nativeOrder());
            bb.putInt(80);
            bb.putInt(25);
            return ErlList.fromByteArray(bb.array());
        }

        throw ErlControlException.makeBadarg();
    }
}
