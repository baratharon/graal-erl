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

import java.util.ArrayList;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.MFA;
import com.oracle.truffle.erl.builtins.ErlBuiltinNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.ErlMap;
import com.oracle.truffle.erl.runtime.ErlTuple;

/**
 * The function takes a list of key-value tuples elements and builds a map. The associations may be
 * in any order and both keys and values in the association may be of any term. If the same key
 * appears more than once, the latter (rightmost) value is used and the previous values are ignored.
 */
@NodeInfo(shortName = "fromList")
public abstract class FromListBuiltin extends ErlBuiltinNode {

    public FromListBuiltin() {
        super(SourceSection.createUnavailable("Erlang builtin", "from_list"));
    }

    @Override
    public MFA getName() {
        return new MFA("maps", "from_list", 1);
    }

    @Specialization
    public ErlMap fromList(ErlList list) {

        ArrayList<ErlMap.Assoc> assocs = new ArrayList<>();

        ErlList tail = list;

        while (ErlList.NIL != tail) {

            if (!(tail.getHead() instanceof ErlTuple)) {
                throw ErlControlException.makeBadarg();
            }

            ErlTuple tuple = (ErlTuple) tail.getHead();

            if (2 != tuple.getSize()) {
                throw ErlControlException.makeBadarg();
            }

            assocs.add(new ErlMap.Assoc(tuple.getElement(1), tuple.getElement(2)));

            final Object next = tail.getTail();

            if (next instanceof ErlList) {
                tail = (ErlList) next;
            } else {
                throw ErlControlException.makeBadarg();
            }
        }

        return ErlMap.fromArray(assocs.toArray(new ErlMap.Assoc[assocs.size()]));
    }

    @Specialization
    public ErlMap fromList(Object arg) {

        return fromList(ErlList.fromObject(arg));
    }
}
