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
import com.oracle.truffle.erl.builtins.ErlBuiltinNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlBinary;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.MFA;
import com.oracle.truffle.erl.runtime.ErlTuple;
import com.oracle.truffle.erl.runtime.misc.ExternalTerm;

/**
 * Returns a binary data object which is the result of encoding Term according to the Erlang
 * external term format.
 * <p>
 * This can be used for a variety of purposes, for example writing a term to a file in an efficient
 * way, or sending an Erlang term to some type of communications channel not supported by
 * distributed Erlang.
 */
@NodeInfo(shortName = "termToBinary")
public abstract class TermToBinary2Builtin extends ErlBuiltinNode {

    public TermToBinary2Builtin() {
        super(SourceSection.createUnavailable("Erlang builtin", "term_to_binary"));
    }

    @Override
    public MFA[] getNames() {
        return new MFA[]{new MFA("erlang", "term_to_binary", 2)};
    }

    private static final int MINOR_VERSION = 0;
    private static final int COMPRESS_LEVEL = 1;
    private static final int OPTION_COUNT = 2;

    @Specialization
    public ErlBinary termToBinary(Object term, ErlList opts) {

        int[] options = new int[OPTION_COUNT];
        decodeOptions(opts, options);

        final byte[] bytes = ExternalTerm.toBinary(term, options[MINOR_VERSION], options[COMPRESS_LEVEL]);

        if (null != bytes) {
            return ErlBinary.fromArray(bytes);
        }

        throw ErlControlException.makeBadarg();
    }

    @Specialization
    public ErlBinary termToBinary(Object term, Object opts) {
        return termToBinary(term, ErlList.fromObject(opts));
    }

    private static void decodeOptions(ErlList opts_, int[] options) {

        assert OPTION_COUNT == options.length;

        ErlList opts = opts_;

        options[MINOR_VERSION] = ExternalTerm.DEFAULT_MINOR_VERSION;
        options[COMPRESS_LEVEL] = ExternalTerm.NO_COMPRESSION;

        while (ErlList.NIL != opts) {

            final Object head = opts.getHead();

            if (head instanceof ErlTuple) {

                final ErlTuple tuple = (ErlTuple) head;

                if (2 == tuple.getSize()) {

                    if (ErlAtom.COMPRESSED.equals(tuple.getElement(1))) {
                        int level = ErlContext.decodeInt(tuple.getElement(2));
                        if (0 <= level && level <= 9) {
                            options[COMPRESS_LEVEL] = level;
                        } else {
                            throw ErlControlException.makeBadarg();
                        }
                    } else if (ErlAtom.MINOR_VERSION.equals(tuple.getElement(1))) {
                        int ver = ErlContext.decodeInt(tuple.getElement(2));
                        if (0 <= ver && ver <= 1) {
                            options[MINOR_VERSION] = ver;
                        } else {
                            throw ErlControlException.makeBadarg();
                        }
                    } else {
                        throw ErlControlException.makeBadarg();
                    }

                } else {
                    throw ErlControlException.makeBadarg();
                }

            } else if (ErlAtom.COMPRESSED.equals(head)) {
                options[COMPRESS_LEVEL] = ExternalTerm.DEFAULT_COMPRESSION_LEVEL;
            } else {
                throw ErlControlException.makeBadarg();
            }

            opts = opts.getTailList();
        }
    }
}
