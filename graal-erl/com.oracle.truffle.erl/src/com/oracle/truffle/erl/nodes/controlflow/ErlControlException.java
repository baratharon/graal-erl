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
package com.oracle.truffle.erl.nodes.controlflow;

import com.oracle.truffle.api.nodes.ControlFlowException;
import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.ErlProcess;
import com.oracle.truffle.erl.runtime.ErlTuple;

/**
 * Exception thrown when an operation failed.
 */
public final class ErlControlException extends ControlFlowException {

    private static final long serialVersionUID = 8195182318498135187L;

    public static enum SpecialTag {

        GENERIC,
        EXIT,
        THROW,

        BADARITH,
        BADARG,
        FUNCTION_CLAUSE,
        IF_CLAUSE,
        UNDEF,
        TIMEOUT_VALUE,
        NOPROC,
        SYSTEM_LIMIT,

        BADMATCH,
        BADFUN,
        BADARITY,
        CASE_CLAUSE,
        TRY_CLAUSE,
        NOCATCH,
        BAD_GENERATOR,
        BAD_FILTER,
        BADKEY,
        BADMAP,
    }

    private final ErlTuple describingTerm;
    private final SpecialTag specialTag;

    private ErlControlException(ErlTuple describingTerm, SpecialTag specialTag) {
        this.describingTerm = describingTerm;
        this.specialTag = specialTag;
    }

    private ErlControlException(ErlTuple describingTerm) {
        this.describingTerm = describingTerm;
        this.specialTag = SpecialTag.GENERIC;
    }

    public static ErlControlException createError(Object desc) {
        return createError(desc, SpecialTag.GENERIC);
    }

    private static ErlControlException createError(Object desc, SpecialTag specialTag) {
        return new ErlControlException(new ErlTuple(ErlAtom._EXIT, new ErlTuple(desc, ErlList.NIL)), specialTag);
        // return new ErlControlException(new ErlTuple(ErlAtom._EXIT, new ErlTuple(desc,
        // ErlProcess.buildBackTraceAsList())), specialTag);
    }

    public static ErlControlException createExit(Object desc) {
        return new ErlControlException(new ErlTuple(ErlAtom._EXIT, desc), SpecialTag.EXIT);
    }

    public static ErlControlException createThrow(Object desc) {
        return new ErlControlException(new ErlTuple(ErlAtom.THROW, desc, ErlList.NIL), SpecialTag.THROW);
    }

    public static ErlControlException makeUndef(Object desc) {
        return createError(new ErlTuple(new ErlAtom("undef"), desc), SpecialTag.UNDEF);
    }

    public static ErlControlException makeBadarith() {
        return createError(new ErlAtom("badarith"), SpecialTag.BADARITH);
    }

    public static ErlControlException makeBadarg() {
        return createError(new ErlAtom("badarg"), SpecialTag.BADARG);
    }

    public static ErlControlException makeBadfun(Object desc) {
        return createError(new ErlTuple(new ErlAtom("badfun"), desc), SpecialTag.BADFUN);
    }

    public static ErlControlException makeBadarity(Object desc) {
        return createError(new ErlTuple(new ErlAtom("badarity"), desc), SpecialTag.BADARITY);
    }

    public static ErlControlException makeBadmatch(Object desc) {
        return createError(new ErlTuple(ErlAtom.BADMATCH, desc), SpecialTag.BADMATCH);
    }

    public static ErlControlException makeUndef() {
        return createError(new ErlAtom("undef"), SpecialTag.UNDEF);
    }

    public static ErlControlException makeFunctionClause() {
        return createError(new ErlAtom("function_clause"), SpecialTag.FUNCTION_CLAUSE);
    }

    public static ErlControlException makeIfClause() {
        return createError(new ErlAtom("if_clause"), SpecialTag.IF_CLAUSE);
    }

    public static ErlControlException makeCaseClause(Object desc) {
        return createError(new ErlTuple(new ErlAtom("case_clause"), desc), SpecialTag.CASE_CLAUSE);
    }

    public static ErlControlException makeTryClause(Object desc) {
        return createError(new ErlTuple(new ErlAtom("try_clause"), desc), SpecialTag.TRY_CLAUSE);
    }

    public static ErlControlException makeNocatch(Object desc) {
        return createError(new ErlTuple(new ErlAtom("nocatch"), desc), SpecialTag.NOCATCH);
    }

    public static ErlControlException makeBadGenerator(Object desc) {
        return createError(new ErlTuple(ErlAtom.BAD_GENERATOR, desc), SpecialTag.BAD_GENERATOR);
    }

    public static ErlControlException makeBadFilter(Object desc) {
        return createError(new ErlTuple(ErlAtom.BAD_FILTER, desc), SpecialTag.BAD_FILTER);
    }

    public static ErlControlException makeBadkey(Object desc) {
        return createError(new ErlTuple(ErlAtom.BADKEY, desc), SpecialTag.BADKEY);
    }

    public static ErlControlException makeBadmap(Object desc) {
        return createError(new ErlTuple(ErlAtom.BADMAP, desc), SpecialTag.BADMAP);
    }

    public static ErlControlException makeTimeoutValue() {
        return createError(new ErlAtom("timeout_value"), SpecialTag.TIMEOUT_VALUE);
    }

    public static ErlControlException makeNoproc() {
        return createError(new ErlAtom("noproc"), SpecialTag.NOPROC);
    }

    public static ErlControlException makeSystemLimit() {
        return createError(new ErlAtom("system_limit"), SpecialTag.SYSTEM_LIMIT);
    }

    public ErlTuple getDescribingTerm() {
        return describingTerm;
    }

    public SpecialTag getSpecialTag() {
        return specialTag;
    }
}
