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

import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlList;

public final class RegexOptions {

    private static final int DEFAULT_MATCH_LIMIT = 10000000;
    private static final int DEFAULT_MATCH_LIMIT_RECURSION = 10000000;

    // compile options:
    private boolean unicode = false;
    private boolean caseless = false;
    private boolean dollarEndonly = false;
    private boolean dotall = false;
    private boolean extended = false;
    private boolean firstline = false;
    private boolean multiline = false;
    private boolean noAutoCapture = false;
    private boolean dupnames = false;
    private boolean ungreedy = false;
    private boolean noStartOptimize = false;
    private boolean ucp = false;
    private boolean neverUtf = false;

    // run options:
    private boolean global = false;
    private boolean notbol = false;
    private boolean noteol = false;
    private boolean notempty = false;
    private boolean notemptyAtstart = false;
    private int offset = 0;
    private int matchLimit = DEFAULT_MATCH_LIMIT;
    private int matchLimitRecursion = DEFAULT_MATCH_LIMIT_RECURSION;
    private CaptureValueSpec captureValueSpec = CaptureValueSpec.ALL;
    private CaptureType captureType = CaptureType.INDEX;

    // both:
    private boolean anchored = false;
    private boolean bsrAnycrlf = false;
    private boolean bsrUnicode = false;
    private NewlineSpec newline = NewlineSpec.LF;

    // ----------------

    private RegexOptions() {
    }

    public RegexOptions update(ErlList options, final boolean compileOptionsOnly) {

        ErlList opts = options;

        while (ErlList.NIL != opts) {

            Object hd = opts.getHead();

            if (ErlAtom.UNICODE.equals(hd)) {
                unicode = true;
            } else if (ErlAtom.ANCHORED.equals(hd)) {
                anchored = true;

            } else if (ErlAtom.GLOBAL.equals(hd) && !compileOptionsOnly) {
                global = true;

            } else {
                throw ErlControlException.makeBadarg();
            }

            opts = opts.getTailList();
        }

        return this;
    }

    public static RegexOptions parse(final ErlList options, final boolean compileOptionsOnly) {

        RegexOptions ro = new RegexOptions();
        return ro.update(options, compileOptionsOnly);
    }

    public boolean unicode() {
        return unicode;
    }

    public boolean anchored() {
        return anchored;
    }

    public boolean caseless() {
        return caseless;
    }

    public boolean dollarEndonly() {
        return dollarEndonly;
    }

    public boolean dotall() {
        return dotall;
    }

    public boolean extended() {
        return extended;
    }

    public boolean firstline() {
        return firstline;
    }

    public boolean multiline() {
        return multiline;
    }

    public boolean noAutoCapture() {
        return noAutoCapture;
    }

    public boolean dupnames() {
        return dupnames;
    }

    public boolean ungreedy() {
        return ungreedy;
    }

    public NewlineSpec newline() {
        return newline;
    }

    public boolean bsrAnycrlf() {
        return bsrAnycrlf;
    }

    public boolean bsrUnicode() {
        return bsrUnicode;
    }

    public boolean noStartOptimize() {
        return noStartOptimize;
    }

    public boolean ucp() {
        return ucp;
    }

    public boolean neverUtf() {
        return neverUtf;
    }

    public boolean global() {
        return global;
    }

    public boolean notbol() {
        return notbol;
    }

    public boolean noteol() {
        return noteol;
    }

    public boolean notempty() {
        return notempty;
    }

    public boolean notemptyAtstart() {
        return notemptyAtstart;
    }

    public int offset() {
        return offset;
    }

    public int matchLimit() {
        return matchLimit;
    }

    public int matchLimitRecursion() {
        return matchLimitRecursion;
    }

    public CaptureValueSpec captureValueSpec() {
        return captureValueSpec;
    }

    public CaptureType captureType() {
        return captureType;
    }
}
