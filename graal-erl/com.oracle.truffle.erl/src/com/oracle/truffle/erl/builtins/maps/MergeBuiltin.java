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
package com.oracle.truffle.erl.builtins.maps;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.MFA;
import com.oracle.truffle.erl.builtins.ErlBuiltinNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlMap;

/**
 * Merges two maps into a single map Map3. If two keys exists in both maps the value in Map1 will be
 * superseded by the value in Map2.
 * <p>
 * The call will fail with a {badmap,Map} exception if Map1 or Map2 is not a map.
 */
@NodeInfo(shortName = "merge")
public abstract class MergeBuiltin extends ErlBuiltinNode {

    public MergeBuiltin() {
        super(SourceSection.createUnavailable("Erlang builtin", "merge"));
    }

    @Override
    public MFA getName() {
        return new MFA("maps", "merge", 2);
    }

    @Specialization
    public ErlMap merge(ErlMap lhs, ErlMap rhs) {

        final Object[] keys = rhs.getKeyArray();
        final Object[] vals = rhs.getValueArray();
        final ErlMap.Assoc[] assocs = new ErlMap.Assoc[keys.length];

        assert keys.length == vals.length;

        for (int i = 0; i < keys.length; ++i) {
            assocs[i] = new ErlMap.Assoc(keys[i], vals[i], false);
        }

        return lhs.makeUpdated(assocs);
    }

    @Specialization
    public ErlMap merge(Object arg1, Object arg2) {

        if (arg1 instanceof ErlMap) {

            if (arg2 instanceof ErlMap) {
                return merge((ErlMap) arg1, (ErlMap) arg2);
            }

            throw ErlControlException.makeBadmap(arg2);
        }

        throw ErlControlException.makeBadmap(arg1);
    }
}
