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
 * Returns a tuple which is a copy of the argument Tuple1 with the element given by the integer
 * argument Index (the first element is the element with index 1) replaced by the argument Value.
 */
@NodeInfo(shortName = "setelement")
public abstract class SetelementBuiltin extends ErlBuiltinNode {

    public SetelementBuiltin() {
        super(SourceSection.createUnavailable("Erlang builtin", "setelement"));
    }

    @Override
    public MFA getName() {
        return new MFA("erlang", "setelement", 3);
    }

    @Specialization
    public ErlTuple setelement(long index, ErlTuple value, Object elem) {

        return value.makeChanged(index, elem);
    }

    @Specialization
    public ErlTuple setelement(BigInteger index, ErlTuple value, Object elem) {

        try {
            return value.makeChanged(index.intValueExact(), elem);
        } catch (ArithmeticException ex) {
            throw ErlControlException.makeBadarg();
        }
    }

    @Specialization
    public ErlTuple setelement(Object index, Object value, Object elem) {

        if (value instanceof ErlTuple) {

            if (index instanceof Long) {
                return setelement((long) index, (ErlTuple) value, elem);
            } else if (index instanceof BigInteger) {
                return setelement((BigInteger) index, (ErlTuple) value, elem);
            }
        }

        throw ErlControlException.makeBadarg();
    }
}
