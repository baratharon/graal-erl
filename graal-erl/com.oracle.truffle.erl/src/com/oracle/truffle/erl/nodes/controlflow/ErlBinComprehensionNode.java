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

import java.util.ArrayList;
import java.util.List;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.nodes.ErlExpressionNode;
import com.oracle.truffle.erl.runtime.ErlBinary;
import com.oracle.truffle.erl.runtime.ErlBinaryView;
import com.oracle.truffle.erl.runtime.ErlLazyBinary;

/**
 * TODO
 */
@NodeInfo(shortName = "<<||>>", description = "")
public final class ErlBinComprehensionNode extends ErlListComprehensionNode {

    public ErlBinComprehensionNode(SourceSection src, ErlExpressionNode exprNode, ErlExpressionNode[] qualifierNodes) {
        super(src, exprNode, qualifierNodes);

    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {

        List<Object> elements = new ArrayList<>();
        build(frame, elements, 0);

        switch (elements.size()) {
            case 0: {
                return ErlBinary.EMPTY;
            }

            case 1: {

                Object obj = elements.get(0);

                if (!(obj instanceof ErlBinary) && !(obj instanceof ErlLazyBinary) && !(obj instanceof ErlBinaryView)) {
                    throw ErlControlException.makeBadarg();
                }

                return obj;
            }

            default: {

                List<ErlLazyBinary> lazy = new ArrayList<>();

                for (Object obj : elements) {

                    if (obj instanceof ErlLazyBinary) {
                        lazy.add((ErlLazyBinary) obj);
                    } else if (obj instanceof ErlBinary) {
                        lazy.add(new ErlLazyBinary((ErlBinary) obj));
                    } else {
                        throw ErlControlException.makeBadarg();
                    }
                }

                while (1 != lazy.size()) {

                    List<ErlLazyBinary> tmp = new ArrayList<>();
                    for (int i = 0; i + 1 < lazy.size(); i += 2) {
                        tmp.add(new ErlLazyBinary(lazy.get(i), lazy.get(i + 1)));
                    }

                    if (0 != (lazy.size() & 1)) {
                        tmp.add(lazy.get(lazy.size() - 1));
                    }

                    lazy = tmp;
                }

                return lazy.get(0);
            }
        }
    }
}
