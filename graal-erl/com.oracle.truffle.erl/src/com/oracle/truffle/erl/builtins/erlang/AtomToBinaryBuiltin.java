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

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.MFA;
import com.oracle.truffle.erl.builtins.ErlBuiltinNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlBinary;
import com.oracle.truffle.erl.runtime.ErlContext;

/**
 * Returns a binary which corresponds to the text representation of Atom. If Encoding is latin1,
 * there will be one byte for each character in the text representation. If Encoding is utf8 or
 * unicode, the characters will be encoded using UTF-8 (meaning that characters from 16#80 up to
 * 0xFF will be encoded in two bytes).
 */
@NodeInfo(shortName = "atomToBinary")
public abstract class AtomToBinaryBuiltin extends ErlBuiltinNode {

    public AtomToBinaryBuiltin() {
        super(SourceSection.createUnavailable("Erlang builtin", "atom_to_binary"));
    }

    @Override
    public MFA[] getNames() {
        return new MFA[]{new MFA("erlang", "atom_to_binary", 2)};
    }

    @Specialization
    public ErlBinary atomToBinary(ErlAtom atom, ErlAtom encoding) {

        if (ErlAtom.LATIN1.equals(encoding)) {
            return ErlContext.stringToBinary(atom.getValue(), ErlContext.LATIN1_CHARSET);
        } else if (ErlAtom.UNICODE.equals(encoding) || ErlAtom.UTF8.equals(encoding)) {
            return ErlContext.stringToBinary(atom.getValue(), ErlContext.UTF8_CHARSET);
        }

        throw ErlControlException.makeBadarg();
    }

    @Specialization
    public ErlBinary atomToBinary(Object arg1, Object arg2) {
        return atomToBinary(ErlAtom.fromObject(arg1), ErlAtom.fromObject(arg2));
    }
}
