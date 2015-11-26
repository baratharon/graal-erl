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

import java.util.HashSet;

import com.oracle.truffle.api.interop.ForeignAccess;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.misc.Registrable;

/**
 * The Erlang ports are represented as numbers. This class is a valid Erlang term, and an rich
 * object as well. Operations are performed directly on the port.
 */
public abstract class ErlPort implements TruffleObject, Registrable {

    private final long id;
    private final String portName;
    private String registeredName = null;
    private ErlPid ownerPid;
    private ErlPid sendClosedTo;
    private HashSet<ErlPid> links = new HashSet<>();
    private boolean closing = false;

    private static long nextPortId = 1;

    public synchronized static long makeId() {
        return nextPortId++;
    }

    protected ErlPort(String name) {
        this.id = makeId();
        this.portName = name;
        this.ownerPid = ErlProcess.getSelfPid();
        this.sendClosedTo = this.ownerPid;
        this.links.add(this.ownerPid);
    }

    public static ErlPort fromObject(Object arg) {

        if (arg instanceof ErlPort) {
            return (ErlPort) arg;
        }

        throw ErlControlException.makeBadarg();
    }

    public long getId() {
        return id;
    }

    public String getName() {
        return portName;
    }

    @Override
    public String getRegisteredName() {
        return registeredName;
    }

    @Override
    public void setRegisteredName(String registeredName) {
        this.registeredName = registeredName;
    }

    @Override
    public void onUnregistered() {
        registeredName = null;
    }

    @Override
    public String toString() {
        return "#Port<" + (id >>> 32) + "." + (id & 0xFFFFFFFF) + ">";
    }

    public int compare(ErlPort rhs) {
        return Long.compare(this.id, rhs.id);
    }

    @Override
    public boolean equals(Object rhs) {
        if (rhs instanceof ErlPort) {
            return 0 == compare((ErlPort) rhs);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return (int) (id ^ (id >>> 32));
    }

    @Override
    public ForeignAccess getForeignAccess() {
        return ErlFunctionForeignAccess.create();
    }

    private void cleanup() {
        links.clear();
        ErlProcess.unregister(registeredName);
    }

    public boolean closeSync() {
        sendClosedTo = null;
        closeUnderlying();
        flush();
        cleanup();
        return true;
    }

    public boolean closeAsync(ErlPid sender) {

        if (closing) {
            return true;
        }

        sendClosedTo = sender;
        closing = true;

        closeUnderlying();
        step();
        return true;
    }

    protected void onClosed() {
        final ErlTuple msg = new ErlTuple(this, ErlAtom.CLOSED);
        ErlProcess.send(sendClosedTo, msg, false, false);
        cleanup();
    }

    protected abstract void step();

    public abstract void flush();

    protected abstract void closeUnderlying();

    public abstract boolean isOpen();

    public abstract boolean command(final ErlPid sender, final byte[] data, boolean nosuspend);

    public abstract Object control(int operation, byte[] data);
}
