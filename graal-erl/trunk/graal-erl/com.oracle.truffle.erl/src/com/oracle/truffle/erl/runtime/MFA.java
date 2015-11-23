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
package com.oracle.truffle.erl.runtime;

/**
 * Class to hold a Module:Function/Arity name. Used as key in maps. Also for printing purposes and
 * for building back trace information.
 */
public final class MFA {

    private final String module;
    private final String function;
    private final int arity;
    private ErlTuple cachedTuple = null;

    public MFA(String module, String function, int arity) {
        this.module = module;
        this.function = function;
        this.arity = arity;

        assert null != module && !module.isEmpty();
        assert null != function && !function.isEmpty();
    }

    public String getModule() {
        return module;
    }

    public String getFunction() {
        return function;
    }

    public int getArity() {
        return arity;
    }

    @Override
    public String toString() {
        return module + ":" + function + "/" + arity;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + arity;
        result = prime * result + ((module == null) ? 0 : module.hashCode());
        result = prime * result + ((function == null) ? 0 : function.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (null != obj && obj instanceof MFA) {
            MFA rhs = (MFA) obj;

            return arity == rhs.arity && module.equals(rhs.module) && function.equals(rhs.function);
        }

        return false;
    }

    public ErlTuple toTuple() {

        if (null == cachedTuple) {
            cachedTuple = new ErlTuple(new ErlAtom(module), new ErlAtom(function), (long) arity, ErlList.NIL);
        }

        return cachedTuple;
    }
}
