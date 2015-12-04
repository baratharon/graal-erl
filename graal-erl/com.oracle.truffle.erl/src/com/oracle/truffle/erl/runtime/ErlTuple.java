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

import java.io.ByteArrayOutputStream;
import java.util.Arrays;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.interop.ForeignAccess;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.misc.ExternalTerm;

/**
 * The Erlang tuples are represented as array of objects.
 */
public final class ErlTuple implements TruffleObject {

    final private Object[] elements;

    public static final ErlTuple EMPTY = new ErlTuple(new Object[]{});
    public static final ErlTuple EXIT_NORMAL = new ErlTuple(ErlAtom._EXIT, ErlAtom.NORMAL);
    public static final ErlTuple EXIT_KILL = new ErlTuple(ErlAtom._EXIT, ErlAtom.KILL);
    public static final ErlTuple EXIT_KILLED = new ErlTuple(ErlAtom._EXIT, ErlAtom.KILLED);

    public ErlTuple(Object... elements) {

        assert null != elements;
        this.elements = elements;

        for (Object obj : elements) {
            assert null != obj;
        }
    }

    public static ErlTuple fromArray(Object[] elements) {
        return new ErlTuple(elements);
    }

    public static ErlTuple fromObject(Object obj) {

        if (obj instanceof ErlTuple) {
            return (ErlTuple) obj;
        }

        throw ErlControlException.makeBadarg();
    }

    public ErlTuple makeAppended(Object element) {
        Object[] elems = Arrays.copyOf(elements, elements.length + 1);
        elems[elements.length] = element;
        return new ErlTuple(elems);
    }

    public ErlTuple makeDeleted(long index) {

        if (index <= 0 || index > elements.length) {
            throw ErlControlException.makeBadarg();
        }

        Object[] elems = Arrays.copyOf(elements, elements.length - 1);
        int idx = (int) index - 1;

        while (idx < elems.length) {
            elems[idx] = elements[idx + 1];
            ++idx;
        }

        return new ErlTuple(elems);
    }

    public ErlTuple makeInserted(long index, Object element) {

        if (index <= 0 || index > elements.length + 1) {
            throw ErlControlException.makeBadarg();
        }

        Object[] elems = Arrays.copyOf(elements, elements.length + 1);

        int idx = (int) index;

        elems[idx - 1] = element;
        while (idx < elems.length) {
            elems[idx] = elements[idx - 1];
            ++idx;
        }

        return new ErlTuple(elems);
    }

    public ErlTuple makeChanged(long index, Object element) {

        if (index <= 0 || index > elements.length) {
            throw ErlControlException.makeBadarg();
        }

        Object[] elems = Arrays.copyOf(elements, elements.length);
        elems[(int) index - 1] = element;

        return new ErlTuple(elems);
    }

    @Override
    public String toString() {

        StringBuilder sb = new StringBuilder();
        String sep = "";

        sb.append('{');

        for (Object element : elements) {
            sb.append(sep);
            sb.append(element);
            sep = ",";
        }

        sb.append('}');

        return sb.toString();
    }

    public int getSize() {
        return elements.length;
    }

    public Object getElement(int idx) {
        if (0 < idx && idx <= elements.length) {
            return elements[idx - 1];
        }

        throw ErlControlException.makeBadarg();
    }

    @TruffleBoundary
    public int compare(ErlTuple rhs, boolean exact) {

        final ErlTuple lhs = this;
        final int length = lhs.getSize();

        if (length < rhs.getSize()) {
            return -1;
        } else if (length > rhs.getSize()) {
            return 1;
        }

        for (int i = 0; i < length; ++i) {

            final int result = ErlContext.compareTerms(lhs.elements[i], rhs.elements[i], exact);

            if (0 != result) {
                return result;
            }
        }

        return 0;
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }

    @Override
    public boolean equals(Object rhs) {
        if (rhs instanceof ErlTuple) {
            return 0 == compare((ErlTuple) rhs, true);
        }

        return false;
    }

    @Override
    public ForeignAccess getForeignAccess() {
        return ErlFunctionForeignAccess.create();
    }

    public boolean encode(ByteArrayOutputStream out, ExternalTerm.Encoder encoder) {

        if (elements.length <= 255) {

            out.write(ExternalTerm.SMALL_TUPLE_EXT);
            out.write(elements.length);

        } else {

            out.write(ExternalTerm.LARGE_TUPLE_EXT);
            out.write((elements.length >>> 24) & 0xff);
            out.write((elements.length >>> 16) & 0xff);
            out.write((elements.length >>> 8) & 0xff);
            out.write(elements.length & 0xff);
        }

        for (Object elem : elements) {
            if (!encoder.encode(elem)) {
                return false;
            }
        }

        return true;
    }
}
