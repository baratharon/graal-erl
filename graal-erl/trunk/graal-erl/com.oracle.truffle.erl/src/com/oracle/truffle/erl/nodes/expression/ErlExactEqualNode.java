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
package com.oracle.truffle.erl.nodes.expression;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.nodes.ErlBinaryNode;
import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.ErlTuple;

import java.math.BigInteger;

/**
 * This class is similar to the extensively documented {@link ErlAddNode}. The only difference: the
 * specialized methods return {@code boolean} instead of the input types.
 */
@NodeInfo(shortName = "=:=")
public abstract class ErlExactEqualNode extends ErlBinaryNode {

    public ErlExactEqualNode(SourceSection src) {
        super(src);
    }

    @Override
    public abstract boolean executeBoolean(VirtualFrame frame);

    @Specialization
    protected boolean equal(boolean left, boolean right) {
        // Note that, the comparison is based on the text form of "true" and "false".
        return left == right;
    }

    @Specialization
    protected boolean equal(long left, long right) {
        return left == right;
    }

    @Specialization
    @TruffleBoundary
    protected boolean equal(BigInteger left, BigInteger right) {
        return left.compareTo(right) == 0;
    }

    @Specialization
    protected boolean equal(double left, double right) {
        return left == right;
    }

    @Specialization
    protected boolean equal(boolean left, ErlAtom right) {
        return ErlAtom.fromBoolean(left).getValue().compareTo(right.getValue()) == 0;
    }

    @Specialization
    protected boolean equal(ErlAtom left, boolean right) {
        return left.getValue().compareTo(ErlAtom.fromBoolean(right).getValue()) == 0;
    }

    @Specialization
    protected boolean equal(ErlAtom left, ErlAtom right) {
        return left.getValue().compareTo(right.getValue()) == 0;
    }

    @Specialization
    protected boolean equal(ErlTuple left, ErlTuple right) {
        return left.compare(right, true) == 0;
    }

    @Specialization
    protected boolean equal(ErlList left, ErlList right) {
        return left.compare(right, true) == 0;
    }

    @Fallback
    protected boolean equal(Object left, Object right) {
        // the general case is to call the term compare function, and we hope it will return quickly
        // (comparing different data types is quite fast)
        return ErlContext.compareTerms(left, right, true) == 0;
    }
}
