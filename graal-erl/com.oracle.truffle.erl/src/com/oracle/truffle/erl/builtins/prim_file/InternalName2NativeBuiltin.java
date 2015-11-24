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
package com.oracle.truffle.erl.builtins.prim_file;

import java.util.Arrays;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.MFA;
import com.oracle.truffle.erl.builtins.ErlBuiltinNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlBinary;
import com.oracle.truffle.erl.runtime.ErlBinaryView;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlLazyBinary;
import com.oracle.truffle.erl.runtime.ErlList;

/**
 * TODO: undocumented BIF
 */
@NodeInfo(shortName = "internal_name2native")
public abstract class InternalName2NativeBuiltin extends ErlBuiltinNode {

    public InternalName2NativeBuiltin() {
        super(SourceSection.createUnavailable("Erlang builtin", "internal_name2native"));
    }

    @Override
    public MFA[] getNames() {
        return new MFA[]{new MFA("prim_file", "internal_name2native", 1)};
    }

    @Specialization
    public ErlBinary internal_name2native(ErlBinary bin) {

        if (bin.hasByteFragment()) {
            throw ErlControlException.makeBadarg();
        }

        final byte[] bytes = bin.toByteArray();
        return ErlBinary.fromArray(Arrays.copyOf(bytes, bytes.length + 1));
    }

    @Specialization
    public ErlBinary internal_name2native(ErlLazyBinary arg) {
        return internal_name2native(arg.construct());
    }

    @Specialization
    public ErlBinary internal_name2native(ErlBinaryView arg) {
        return internal_name2native(arg.construct());
    }

    @Specialization
    public ErlBinary internal_name2native(ErlList list_) {

        ErlList list = list_;
        final int len = (int) list.length();
        final byte[] bytes = new byte[len + 1];
        int index = 0;
        byte[] tmp = new byte[1];

        while (ErlList.NIL != list) {
            final Object head = list.getHead();

            if (ErlContext.isByte(head, tmp)) {
                bytes[index++] = tmp[0];
            } else {
                throw ErlControlException.makeBadarg();
            }

            list = list.getTailList();
        }

        bytes[len] = (byte) 0;

        return ErlBinary.fromArray(bytes);
    }

    @Specialization
    public ErlBinary internal_name2native(Object arg) {

        if (arg instanceof ErlList) {
            return internal_name2native((ErlList) arg);
        }

        return internal_name2native(ErlBinary.fromObject(arg));
    }
}
