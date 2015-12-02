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

public final class MathExtension {

    // don't instantiate this
    private MathExtension() {
    }

    private static final double LOG_2 = Math.log(2.0);

    public static double log2(double number) {
        return Math.log(number) / LOG_2;
    }

    public static double asinh(double number) {
        return Math.log(number + Math.sqrt(number * number + 1.0));
    }

    public static double acosh(double number) {
        return Math.log(number + Math.sqrt(number * number - 1.0));
    }

    public static double atanh(double number) {
        return 0.5 * Math.log((number + 1.0) / (number - 1.0));
    }

    private static final double ERF_A1 = 0.254829592;
    private static final double ERF_A2 = -0.284496736;
    private static final double ERF_A3 = 1.421413741;
    private static final double ERF_A4 = -1.453152027;
    private static final double ERF_A5 = 1.061405429;
    private static final double ERF_P = 0.3275911;

    /**
     * The code taken from
     * <a href="http://picomath.org/index.html">http://picomath.org/index.html</a>.
     */
    public static double erf(double number) {

        // Save the sign of x
        double sign = 1.0;
        if (number < 0.0) {
            sign = -1.0;
        }
        double x = Math.abs(number);

        // A&S formula 7.1.26
        double t = 1.0 / (1.0 + ERF_P * x);
        double y = 1.0 - (((((ERF_A5 * t + ERF_A4) * t) + ERF_A3) * t + ERF_A2) * t + ERF_A1) * t * Math.exp(-x * x);

        return sign * y;
    }

    public static double erfc(double number) {
        return 1.0 - erf(number);
    }
}
