/*
 * Copyright (c) 2014, Oracle and/or its affiliates. All rights reserved.
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

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.interop.ForeignAccess;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.utilities.CyclicAssumption;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.misc.FunInfoItem;

/**
 * Represents an Erlang function. On the Truffle level, a callable element is represented by a
 * {@link RootCallTarget call target}. This class encapsulates a call target, and adds version
 * support: functions in Erlang can be redefined, i.e. changed at run time. When a function is
 * redefined, the call target managed by this function object is changed (and {@link #callTarget} is
 * therefore not a final field).
 * <p>
 * Function redefinition is expected to be rare, therefore optimized call nodes want to speculate
 * that the call target is stable. This is possible with the help of a Truffle {@link Assumption}: a
 * call node can keep the call target returned by {@link #getCallTarget()} cached until the
 * assumption returned by {@link #getCallTargetStable()} is valid.
 */
public class ErlFunction implements TruffleObject {

    public enum Origin {
        REGULAR,
        BUILTIN,
        ANONYMOUS
    }

    /** The name of the module where the function is. */
    private final String module;

    /** The name of the function. */
    private final String name;

    /** The arity of the function. Some kind of "overload" is possible by using different arity. */
    private final int arity;

    /** The origin of the function. */
    private final Origin origin;

    /** Special arity value to represent unresolved arity. */
    public static final int UNRESOLVED_ARITY = -1;

    /** The current implementation of this function. */
    private RootCallTarget callTarget;

    /**
     * Manages the assumption that the {@link #callTarget} is stable. We use the utility class
     * {@link CyclicAssumption}, which automatically creates a new {@link Assumption} when the old
     * one gets invalidated.
     */
    private final CyclicAssumption callTargetStable;

    protected ErlFunction(String module, String name, int arity, Origin origin) {
        this.module = module;
        this.name = name;
        this.arity = arity;
        this.origin = origin;
        this.callTargetStable = new CyclicAssumption(name);
    }

    public static ErlFunction fromObject(Object obj) {

        if (obj instanceof ErlFunction) {
            return (ErlFunction) obj;
        }

        throw ErlControlException.makeBadarg();
    }

    public String getModule() {
        return module;
    }

    public String getName() {
        return name;
    }

    public int getArity() {
        return arity;
    }

    public Origin getOrigin() {
        return origin;
    }

    public boolean isBuiltin() {
        return Origin.BUILTIN == origin;
    }

    public boolean isAnonymous() {
        return Origin.ANONYMOUS == origin;
    }

    public Object[] getContext() {
        return null;
    }

    public ErlFunction withContext(Object[] context) {
        if (Origin.ANONYMOUS == origin) {
            return new ErlFunctionWithContext(this, context);
        }

        return this;
    }

    protected void setCallTarget(RootCallTarget callTarget) {
        this.callTarget = callTarget;
        /*
         * We have a new call target. Invalidate all code that speculated that the old call target
         * was stable.
         */
        callTargetStable.invalidate();
    }

    public RootCallTarget getCallTarget() {
        return callTarget;
    }

    public Assumption getCallTargetStable() {
        return callTargetStable.getAssumption();
    }

    public boolean isCallable() {
        return null != callTarget;
    }

    /**
     * This method is, e.g., called when using a function literal in a string concatenation. So
     * changing it has an effect on Erlang programs.
     */
    @Override
    public String toString() {
        return "#Fun<" + module + "." + name + "." + arity + ">";
    }

    public int compare(ErlFunction rhs) {

        int res;

        if (0 != (res = Integer.compare(arity, rhs.arity))) {
            return res;
        }

        if (0 != (res = module.compareTo(rhs.module))) {
            return res;
        }

        if (0 != (res = name.compareTo(rhs.name))) {
            return res;
        }

        return 0;
    }

    public ErlTuple getInfo(FunInfoItem item) {
        switch (item) {
            case TYPE: {
                // hack: assume all functions are external functions, so now we don't have to
                // provide lot of (currently) unknown information
                return new ErlTuple(item.atom, ErlAtom.EXTERNAL);
            }

            case NAME: {
                return new ErlTuple(item.atom, new ErlAtom(name));
            }

            case MODULE: {
                return new ErlTuple(item.atom, new ErlAtom(module));
            }

            case ARITY: {
                return new ErlTuple(item.atom, (long) arity);
            }

            case ENV: {
                return new ErlTuple(item.atom, ErlList.NIL);
            }

            default:
                return null;
        }
    }

    /**
     * In case you want some of your objects to co-operate with other languages, you need to make
     * them implement {@link TruffleObject} and provide additional {@link ErlFunctionForeignAccess
     * foreign access implementation}.
     */
    @Override
    public ForeignAccess getForeignAccess() {
        return ErlFunctionForeignAccess.create();
    }
}
