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
package com.oracle.truffle.erl.runtime.ets;

import com.oracle.truffle.erl.runtime.ErlAtom;

public enum TableType {
    /**
     * The table is a set table - one key, one object, no order among objects. This is the default
     * table type.
     */
    SET(ErlAtom.SET),

    /**
     * The table is a ordered_set table - one key, one object, ordered in Erlang term order, which
     * is the order implied by the < and > operators. Tables of this type have a somewhat different
     * behavior in some situations than tables of the other types. Most notably the ordered_set
     * tables regard keys as equal when they compare equal, not only when they match. This means
     * that to an ordered_set, the integer() 1 and the float() 1.0 are regarded as equal. This also
     * means that the key used to lookup an element not necessarily matches the key in the elements
     * returned, if float()'s and integer()'s are mixed in keys of a table.
     */
    ORDERED_SET(ErlAtom.ORDERED_SET),

    /**
     * The table is a bag table which can have many objects, but only one instance of each object,
     * per key.
     */
    BAG(ErlAtom.BAG),

    /**
     * The table is a duplicate_bag table which can have many objects, including multiple copies of
     * the same object, per key.
     */
    DUPLICATE_BAG(ErlAtom.DUPLICATE_BAG);

    public final ErlAtom atom;

    TableType(ErlAtom atom) {
        this.atom = atom;
    }
}
