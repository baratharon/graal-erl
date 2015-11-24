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
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.misc.Hash;

/**
 * Returns a hash value for Term within the range 1..Range. The allowed range is 1..2^27-1.
 * <p>
 * <strong>Warning</strong> This BIF is deprecated as the hash value may differ on different
 * architectures. Also the hash values for integer terms larger than 2^27 as well as large binaries
 * are very poor. The BIF is retained for backward compatibility reasons (it may have been used to
 * hash records into a file), but all new code should use one of the BIFs erlang:phash/2 or
 * erlang:phash2/1,2 instead.
 */
@NodeInfo(shortName = "hash")
public abstract class HashBuiltin extends ErlBuiltinNode {

    public HashBuiltin() {
        super(SourceSection.createUnavailable("Erlang builtin", "hash"));
    }

    @Override
    public MFA[] getNames() {
        return new MFA[]{new MFA("erlang", "hash", 2)};
    }

    private static final int RANGE_MIN = 1;
    private static final int RANGE_MAX = (1 << 27) - 1;

    @Specialization
    public long hash(Object term, Object rangeObj) {

        final long range = ErlContext.decodeLong(rangeObj);
        if (range < RANGE_MIN || range > RANGE_MAX) {
            throw ErlControlException.makeBadarg();
        }

        final long hc = Integer.toUnsignedLong(Hash.makeBrokenHash(term) + 1);
        final long masked = hc % range;

        if (0 != masked) {
            return masked;
        }

        if (0 != hc) {
            return hc / range;
        }

        return 1;
    }
}
