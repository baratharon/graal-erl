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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.StandardSocketOptions;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.util.ArrayList;

import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.ErlPid;
import com.oracle.truffle.erl.runtime.ErlProcess;
import com.oracle.truffle.erl.runtime.misc.PortOptions;

public final class UdpInet extends Driver {

    private int status = 0;
    private byte family = 0;
    private DatagramChannel channel = null;
    private final ArrayList<ErlPid> subscribers = new ArrayList<>();

    public static String getDriverName() {
        return "udp_inet";
    }

    private UdpInet() {
        super(getDriverName());
    }

    public static UdpInet create(@SuppressWarnings("unused") String command, PortOptions po) {
        if (po.isBinary()) {
            return new UdpInet();
        }

        return null;
    }

    @Override
    protected AsyncAction parseCommand(byte[] data) {

        switch (data[0]) {
        }

        System.err.println("udp_inet: command " + Byte.toUnsignedInt(data[0]) + " is not implemented");
        return null;
    }

    @Override
    protected Object doControl(int operation, byte[] data) {

        switch (operation) {
            case INET_REQ_OPEN:
                return reqOpen(data);

            case INET_REQ_SETOPTS:
                return reqSetOpts(data);

            case INET_REQ_GETOPTS:
                return reqGetOpts(data);

            case INET_REQ_SUBSCRIBE:
                return reqSubscribe(data);
        }

        System.err.println("udp_inet: control " + operation + " is not implemented");
        throw ErlControlException.makeBadarg();
    }

    private Object reqOpen(byte[] data) {

        if (0 != (status & INET_F_OPEN)) {
            return new ErlList((long) INET_REP_ERROR, EXBADSEQ);
        }

        if (2 == data.length) {

            family = data[0];
            final byte type = data[1];

            if ((INET_AF_INET == family || INET_AF_INET6 == family) && INET_TYPE_DGRAM == type) {
                try {
                    channel = DatagramChannel.open();
                    return new ErlList((long) INET_REP_OK, ErlList.NIL);
                } catch (IOException e) {
                    return new ErlList((long) INET_REP_ERROR, EPERM);
                }
            }
        }

        return new ErlList((long) INET_REP_ERROR, EINVAL);
    }

    private Object reqGetOpts(byte[] data) {

        if (null != channel) {
            return new ErlList((long) INET_REP_ERROR, EXBADSEQ);
        }

        final ByteBuffer in = ByteBuffer.wrap(data);
        final ByteArrayOutputStream out = new ByteArrayOutputStream();

        while (in.hasRemaining()) {

            final byte option = in.get();
            out.write(option);

            switch (option) {

                case INET_OPT_LINGER: {
                    try {
                        final int linger = channel.getOption(StandardSocketOptions.SO_LINGER);
                        putInt(out, linger);

                    } catch (IOException e) {
                        return new ErlList((long) INET_REP_ERROR, EINVAL);
                    }
                }

                default:
                    return new ErlList((long) INET_REP_ERROR, EINVAL);
            }
        }

        return new ErlList((long) INET_REP_OK, ErlList.fromByteArray(out.toByteArray()));
    }

    @SuppressWarnings("static-method")
    private Object reqSetOpts(byte[] data) {

        final ByteBuffer in = ByteBuffer.wrap(data);

        while (in.hasRemaining()) {
            switch (in.get()) {
                default:
                    return new ErlList((long) INET_REP_ERROR, EINVAL);
            }
        }

        return new ErlList((long) INET_REP_OK, ErlList.NIL);
    }

    private Object reqSubscribe(byte[] data) {
        ByteBuffer out = ByteBuffer.allocate(data.length * 5);

        for (byte b : data) {
            if (INET_SUBS_EMPTY_OUT_Q == b) {

                final int queueSize = getOutQueueSize();

                if (queueSize > 0) {
                    subscribers.add(ErlProcess.getSelfPid());
                }

                out.put(INET_SUBS_EMPTY_OUT_Q);
                out.putInt(queueSize);

            } else {
                return new ErlList((long) INET_REP_ERROR, EINVAL);
            }
        }

        return new ErlList((long) INET_REP_OK, ErlList.fromByteArray(out.array()));
    }

    private static void putInt(ByteArrayOutputStream out, int linger) {
        out.write((linger >>> 24) & 0xff);
        out.write((linger >>> 16) & 0xff);
        out.write((linger >>> 8) & 0xff);
        out.write(linger & 0xff);
    }

    private int getOutQueueSize() {
        return 0;
    }
}
