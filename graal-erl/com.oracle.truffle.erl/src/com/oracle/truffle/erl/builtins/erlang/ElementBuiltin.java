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
package com.oracle.truffle.erl.builtins.erlang;

import java.math.BigInteger;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.MFA;
import com.oracle.truffle.erl.builtins.ErlBuiltinNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlTuple;

/**
 * Extracts an element from a tuple.
 */
@NodeInfo(shortName = "element")
public abstract class ElementBuiltin extends ErlBuiltinNode {

    public ElementBuiltin() {
        super(SourceSection.createUnavailable("Erlang builtin", "element"));
    }

    @Override
    public MFA[] getNames() {
        return new MFA[]{new MFA("erlang", "element", 2)};
    }

    @Specialization
    public Object element(long index, ErlTuple value) {

        if (index > Integer.MAX_VALUE || index < Integer.MIN_VALUE) {
            throw ErlControlException.makeBadarg();
        }

        return value.getElement((int) index);
    }

    @Specialization
    public Object element(BigInteger index, ErlTuple value) {

        try {
            return value.getElement(index.intValueExact());
        } catch (ArithmeticException ex) {
            throw ErlControlException.makeBadarg();
        }
    }

    @Specialization
    public Object element(Object index, Object value) {

        if (value instanceof ErlTuple) {

            if (index instanceof Long) {
                return element((long) index, (ErlTuple) value);
            } else if (index instanceof BigInteger) {
                return element((BigInteger) index, (ErlTuple) value);
            }
        }

        throw ErlControlException.makeBadarg();
    }
}
