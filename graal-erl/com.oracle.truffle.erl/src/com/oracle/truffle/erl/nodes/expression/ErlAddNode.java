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
import com.oracle.truffle.api.ExactMath;
import com.oracle.truffle.api.dsl.ImplicitCast;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.nodes.ErlBinaryNode;
import com.oracle.truffle.erl.nodes.ErlTypes;
import com.oracle.truffle.erl.runtime.ErlContext;

import java.math.BigInteger;

/**
 * Erlang node that performs the "+" operation, which performs addition on arbitrary precision
 * numbers.
 */
@NodeInfo(shortName = "+")
public abstract class ErlAddNode extends ErlBinaryNode {

    public ErlAddNode(SourceSection src) {
        super(src);
    }

    /**
     * Specialization for primitive {@code long} values. This is the fast path of the
     * arbitrary-precision arithmetic. We need to check for overflows of the addition, and switch to
     * the {@link #add(BigInteger, BigInteger) slow path}. Therefore, we use an
     * {@link ExactMath#addExact(long, long) addition method that throws an exception on overflow}.
     * The {@code rewriteOn} attribute on the {@link Specialization} annotation automatically
     * triggers the node rewriting on the exception.
     * <p>
     * In compiled code, {@link ExactMath#addExact(long, long) addExact} is compiled to efficient
     * machine code that uses the processor's overflow flag. Therefore, this method is compiled to
     * only two machine code instructions on the fast path.
     * <p>
     * This specialization is automatically selected by the Truffle DSL if both the left and right
     * operand are {@code long} values.
     */
    @Specialization(rewriteOn = ArithmeticException.class)
    protected long add(long left, long right) {
        return ExactMath.addExact(left, right);
    }

    /**
     * This is the slow path of the arbitrary-precision arithmetic. The {@link BigInteger} type of
     * Java is doing everything we need.
     * <p>
     * This specialization is automatically selected by the Truffle DSL if both the left and right
     * operand are {@link BigInteger} values. Because the type system defines an {@link ImplicitCast
     * implicit conversion} from {@code long} to {@link BigInteger} in
     * {@link ErlTypes#castBigInteger(long)}, this specialization is also taken if the left or the
     * right operand is a {@code long} value. Because the {@link #add(long, long) long}
     * specialization} has the {@code rewriteOn} attribute, this specialization is also taken if
     * both input values are {@code long} values but the primitive addition overflows.
     */
    @Specialization
    @TruffleBoundary
    protected Object add(BigInteger left, BigInteger right) {
        BigInteger result = left.add(right);
        return ErlContext.returnSimplifiedInteger(result);
    }

    @Specialization
    protected double add(double left, double right) {
        return left + right;
    }

    @Specialization
    protected double add(long left, double right) {
        return left + right;
    }

    @Specialization
    protected double add(double left, long right) {
        return left + right;
    }

    @Specialization
    protected double add(BigInteger left, double right) {
        return left.doubleValue() + right;
    }

    @Specialization
    protected double add(double left, BigInteger right) {
        return left + right.doubleValue();
    }
}
