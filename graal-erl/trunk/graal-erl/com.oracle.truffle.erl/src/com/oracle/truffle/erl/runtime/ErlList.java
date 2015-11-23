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
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.oracle.truffle.api.interop.ForeignAccess;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.misc.ExternalTerm;

/**
 * The Erlang lists are represented with its head and tail (like in Erlang).
 */
public final class ErlList implements TruffleObject {

    final private Object head;
    private Object tail;

    public static final ErlList NIL = new ErlList();

    public static ErlList fromObject(Object obj) {

        if (obj instanceof ErlList) {
            return (ErlList) obj;
        }

        throw ErlControlException.makeBadarg();
    }

    public ErlList(Object head, Object tail) {

        assert null != head;
        assert null != tail;
        this.head = head;
        this.tail = tail;
    }

    private ErlList(Object head) {

        assert null != head;
        this.head = head;
        this.tail = null;
    }

    private ErlList() {
        this.head = null;
        this.tail = null;
    }

    public static ErlList fromArray(Object[] elements) {
        ErlList list = NIL;

        for (int i = elements.length - 1; i >= 0; --i) {
            list = new ErlList(elements[i], list);
        }

        return list;
    }

    public static ErlList fromByteArray(byte[] elements) {
        ErlList list = NIL;

        for (int i = elements.length - 1; i >= 0; --i) {
            list = new ErlList(Byte.toUnsignedLong(elements[i]), list);
        }

        return list;
    }

    public static ErlList fromList(List<Object> elements) {
        ErlList list = NIL;

        for (int i = elements.size() - 1; i >= 0; --i) {
            list = new ErlList(elements.get(i), list);
        }

        return list;
    }

    public static <T> ErlList fromIterator(Iterator<T> iterator) {
        ErlList list = NIL;

        while (iterator.hasNext()) {
            list = new ErlList(iterator.next(), list);
        }

        return list;
    }

    @Deprecated
    public static ErlList fromString(final String str) {

        ErlList result = NIL;

        for (int i = str.length() - 1; i >= 0; --i) {
            result = new ErlList((long) str.charAt(i), result);
        }

        return result;
    }

    public Object append(Object newTail) {

        if (NIL == this) {
            return newTail;
        }

        assert null != tail;

        ErlList result = new ErlList(head);
        ErlList prev = result;
        Object remaining = this.tail;

        while (NIL != remaining && (remaining instanceof ErlList)) {

            ErlList list = (ErlList) remaining;

            prev.tail = new ErlList(list.head);
            prev = (ErlList) prev.tail;

            remaining = list.tail;
        }

        if (NIL != remaining) {
            throw ErlControlException.makeBadarg();
        }

        prev.tail = newTail;

        return result;
    }

    public ErlList subtract(ErlList rhs) {

        if (NIL == this) {
            return NIL;
        }

        ErlList sourceList = this;
        ErlList result = null;
        ErlList prev = result;

        Object[] toRemove = rhs.toArray();
        int size = toRemove.length;

        while (NIL != sourceList) {

            int idx = find(toRemove, size, sourceList.head);

            if (0 > idx) {

                if (null != result) {

                    // we already have a list, just append the new item

                    ErlList newTail = new ErlList(sourceList.head);
                    prev.tail = newTail;
                    prev = newTail;

                } else {

                    // we don't have a list yet, create a new one

                    result = new ErlList(sourceList.head);
                    prev = result;
                }
            } else {
                toRemove[idx] = toRemove[--size];
            }

            if (sourceList.tail instanceof ErlList) {

                sourceList = (ErlList) sourceList.tail;

            } else {

                throw ErlControlException.makeBadarg();
            }
        }

        if (null == result) {
            return NIL;
        }

        assert null != prev;

        prev.tail = NIL;

        return result;
    }

    public Object[] toArray() {

        if (NIL == this) {
            return new Object[]{};
        }

        List<Object> result = new ArrayList<>();

        ErlList remain = this;

        while (NIL != remain) {

            result.add(remain.head);

            if (remain.tail instanceof ErlList) {
                remain = (ErlList) remain.tail;
            } else {
                throw ErlControlException.makeBadarg();
            }
        }

        return result.toArray();
    }

    private static int find(Object[] arr, int size, Object element) {

        for (int i = 0; i < size; ++i) {

            if (0 == ErlContext.compareTerms(arr[i], element, true)) {
                return i;
            }
        }

        return -1;
    }

    @Override
    public String toString() {

        if (NIL == this) {
            return "[]";
        }

        StringBuilder sbRaw = new StringBuilder();
        StringBuilder sbStr = new StringBuilder();

        sbRaw.append('[');
        sbRaw.append(head);

        char[] refChar = new char[1];

        if (ErlContext.isPrintableCharacter(head, refChar)) {
            sbStr.append('\"');
            sbStr.append(ErlContext.stringifyPrintableCharacter(refChar[0]));
        } else {
            sbStr = null;
        }

        Object remaining = this.tail;

        while (NIL != remaining && (remaining instanceof ErlList)) {

            ErlList list = (ErlList) remaining;

            sbRaw.append(",");
            sbRaw.append(list.head);

            if (null != sbStr) {
                if (ErlContext.isPrintableCharacter(list.head, refChar)) {
                    sbStr.append(ErlContext.stringifyPrintableCharacter(refChar[0]));
                } else {
                    sbStr = null;
                }
            }

            remaining = list.tail;
        }

        if (NIL != remaining) {
            sbRaw.append("|");
            sbRaw.append(remaining);
            sbStr = null;
        }

        if (null != sbStr) {
            return sbStr.append('\"').toString();
        }

        sbRaw.append(']');

        return sbRaw.toString();
    }

    public Object getHead() {
        return head;
    }

    public Object getTail() {
        return tail;
    }

    public ErlList getTailList() {
        if (tail instanceof ErlList) {
            return (ErlList) tail;
        }

        throw ErlControlException.makeBadarg();
    }

    public int compare(ErlList rhs, boolean exact) {

        final ErlList lhs = this;

        // 'nil' compared to 'nil' is always 'equals'
        if (NIL == lhs && ErlList.NIL == rhs) {
            return 0;
        } else if (NIL == lhs) {
            return -1;
        } else if (NIL == rhs) {
            return +1;
        } else {

            final int result = ErlContext.compareTerms(lhs.head, rhs.head, exact);
            if (0 != result) {
                return result;
            }
        }

        Object lhsTail = lhs.tail;
        Object rhsTail = rhs.tail;

        while (lhsTail instanceof ErlList && rhsTail instanceof ErlList) {

            if (ErlList.NIL == lhsTail && ErlList.NIL == rhsTail) {
                return 0;
            } else if (ErlList.NIL == lhsTail) {
                return -1;
            } else if (ErlList.NIL == rhsTail) {
                return +1;
            }

            final int result = ErlContext.compareTerms(((ErlList) lhsTail).head, ((ErlList) rhsTail).head, exact);

            if (0 != result) {
                return result;
            }

            lhsTail = ((ErlList) lhsTail).tail;
            rhsTail = ((ErlList) rhsTail).tail;
        }

        return ErlContext.compareTerms(lhsTail, rhsTail, exact);
    }

    public boolean startsWith(ErlList prefix, boolean exact) {

        // 'nil' compared to 'nil' is always 'equals'
        if (NIL == this && ErlList.NIL == prefix) {
            return true;
        } else {

            final int result = ErlContext.compareTerms(this.head, prefix.head, exact);
            if (0 != result) {
                return false;
            }
        }

        Object lhsTail = this.tail;
        Object rhsTail = prefix.tail;

        while (lhsTail instanceof ErlList && rhsTail instanceof ErlList) {

            if (ErlList.NIL == rhsTail) {
                return true;
            }

            if (ErlList.NIL == lhsTail) {
                return false;
            }

            final int result = ErlContext.compareTerms(((ErlList) lhsTail).head, ((ErlList) rhsTail).head, exact);

            if (0 != result) {
                return false;
            }

            lhsTail = ((ErlList) lhsTail).tail;
            rhsTail = ((ErlList) rhsTail).tail;
        }

        return false;
    }

    @Override
    public ForeignAccess getForeignAccess() {
        return ErlFunctionForeignAccess.create();
    }

    public long length() {

        long len = 0;
        ErlList list = this;

        while (ErlList.NIL != list) {
            ++len;

            if (list.tail instanceof ErlList) {
                list = (ErlList) list.tail;
            } else {
                throw ErlControlException.makeBadarg();
            }
        }

        return len;
    }

    public Object nthTail(long index_) {
        long index = index_;
        Object tl = this;

        while (index > 0 && ErlList.NIL != tl && (tl instanceof ErlList)) {
            --index;

            tl = ((ErlList) tl).tail;
        }

        if (index > 0) {
            return null;
        }

        return tl;
    }

    public boolean encode(ByteArrayOutputStream out, ExternalTerm.Encoder encoder) {

        if (NIL == this) {
            out.write(ExternalTerm.NIL_EXT);
            return true;
        }

        int len = 0;
        {
            Object hd = this;
            while (NIL != hd && hd instanceof ErlList) {
                ++len;
                hd = ((ErlList) hd).getTail();
            }
        }

        out.write(ExternalTerm.LIST_EXT);
        out.write((len >>> 24) & 0xff);
        out.write((len >>> 16) & 0xff);
        out.write((len >>> 8) & 0xff);
        out.write(len & 0xff);

        {
            Object ls = this;

            while (0 != (len--)) {
                if (!encoder.encode(((ErlList) ls).head)) {
                    return false;
                }
                ls = ((ErlList) ls).getTail();
            }

            if (!encoder.encode(ls)) {
                return false;
            }
        }

        return true;
    }
}
