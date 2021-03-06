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

import java.math.BigInteger;
import java.util.LinkedList;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlBinary;
import com.oracle.truffle.erl.runtime.ErlBinaryView;
import com.oracle.truffle.erl.runtime.ErlLazyBinary;
import com.oracle.truffle.erl.runtime.ErlList;

/**
 * Non-recursive visitor for iodata() Erlang type concept.
 */
public abstract class IODataVisitor {

    private final static class ListVisitor {

        private Object current;

        public ListVisitor(Object list) {
            this.current = list;
        }

        public Object next() {

            Object result;

            if (current instanceof ErlList) {

                ErlList list = (ErlList) current;

                result = list.getHead();
                current = list.getTail();

            } else {
                result = current;
                current = null;
            }

            return result;
        }
    }

    LinkedList<ListVisitor> stack = new LinkedList<>();

    public IODataVisitor() {
    }

    @TruffleBoundary
    public IODataVisitor accept(Object data) {

        if (!(data instanceof ErlList) && !(data instanceof ErlBinary) && !(data instanceof ErlLazyBinary) && !(data instanceof ErlBinaryView)) {
            throw ErlControlException.makeBadarg();
        }

        process(data);

        while (!stack.isEmpty()) {

            Object obj = stack.getLast().next();

            if (null != obj) {
                process(obj);
            } else {
                stack.removeLast();
            }
        }

        return this;
    }

    private void process(Object data) {
        if (data instanceof ErlList) {
            stack.add(new ListVisitor(data));
        } else if (data instanceof ErlBinary) {

            if (((ErlBinary) data).hasByteFragment()) {
                throw ErlControlException.makeBadarg();
            }

            visit(((ErlBinary) data).toByteArray());

        } else if (data instanceof ErlLazyBinary) {

            ErlBinary bin = ((ErlLazyBinary) data).construct();

            if (bin.hasByteFragment()) {
                throw ErlControlException.makeBadarg();
            }

            visit(bin.toByteArray());

        } else if (data instanceof ErlBinaryView) {

            ErlBinary bin = ((ErlBinaryView) data).construct();

            if (bin.hasByteFragment()) {
                throw ErlControlException.makeBadarg();
            }

            visit(bin.toByteArray());

        } else if (data instanceof Long) {

            final long number = (long) data;

            if (0 <= number && number <= 255) {
                visit((byte) number);
            } else {
                throw ErlControlException.makeBadarg();
            }

        } else if (data instanceof BigInteger) {

            try {
                final long number = ((BigInteger) data).longValueExact();

                if (0 <= number && number <= 255) {
                    visit((byte) number);
                } else {
                    throw ErlControlException.makeBadarg();
                }

            } catch (ArithmeticException ex) {
                throw ErlControlException.makeBadarg();
            }

        } else {
            throw ErlControlException.makeBadarg();
        }
    }

    protected void visit(final byte[] bs) {
        for (byte b : bs) {
            visit(b);
        }
    }

    protected abstract void visit(byte b);
}
