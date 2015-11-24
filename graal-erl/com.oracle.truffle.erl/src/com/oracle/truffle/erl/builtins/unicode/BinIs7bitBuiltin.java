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
package com.oracle.truffle.erl.builtins.unicode;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.MFA;
import com.oracle.truffle.erl.builtins.ErlBuiltinNode;
import com.oracle.truffle.erl.runtime.ErlBinary;
import com.oracle.truffle.erl.runtime.ErlBinaryView;
import com.oracle.truffle.erl.runtime.ErlLazyBinary;

/**
 * Behaves as characters_to_list/2, but produces an binary instead of a Unicode list. The InEncoding
 * defines how input is to be interpreted if binaries are present in the Data, while OutEncoding
 * defines in what format output is to be generated.
 */
@NodeInfo(shortName = "binIs7bit")
public abstract class BinIs7bitBuiltin extends ErlBuiltinNode {

    public BinIs7bitBuiltin() {
        super(SourceSection.createUnavailable("Erlang builtin", "bin_is_7bit"));
    }

    @Override
    public MFA[] getNames() {
        return new MFA[]{new MFA("unicode", "bin_is_7bit", 1)};
    }

    @Specialization
    public boolean binIs7bit(ErlBinary bin) {

        if (bin.hasByteFragment()) {
            return false;
        }

        for (byte b : bin.toByteArray()) {
            if (Byte.toUnsignedInt(b) > 127) {
                return false;
            }
        }

        return true;
    }

    @Specialization
    public boolean binIs7bit(ErlLazyBinary data) {
        return binIs7bit(data.construct());
    }

    @Specialization
    public boolean binIs7bit(ErlBinaryView data) {
        return binIs7bit(data.construct());
    }

    @Specialization
    public boolean binIs7bit(Object data) {
        return binIs7bit(ErlBinary.fromObject(data));
    }
}
