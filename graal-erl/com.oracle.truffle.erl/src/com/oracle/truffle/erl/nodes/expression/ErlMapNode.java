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

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.nodes.ErlExpressionNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlMap;

/**
 * Node that constructs a tuple.
 */
@NodeInfo(shortName = "map")
public final class ErlMapNode extends ErlExpressionNode {

    @Child private ErlExpressionNode optionalMapNode;
    @Children private final ErlExpressionNode[] elementNodes;

    public ErlMapNode(SourceSection src, ErlExpressionNode optionalMapNode, ErlExpressionNode... elementNodes) {
        super(src);
        this.optionalMapNode = optionalMapNode;
        this.elementNodes = elementNodes;
    }

    @Override
    @ExplodeLoop
    public Object executeGeneric(VirtualFrame frame) {

        ErlMap baseMap = null;
        if (null != optionalMapNode) {

            final Object result = optionalMapNode.executeGeneric(frame);
            if (result instanceof ErlMap) {
                baseMap = (ErlMap) result;
            } else {
                throw ErlControlException.makeBadmap(result);
            }
        }

        /*
         * This assertion illustrates that the array length is really a constant during compilation.
         */
        CompilerAsserts.compilationConstant(elementNodes.length);

        ErlMap.Assoc[] actualElements = new ErlMap.Assoc[elementNodes.length];

        for (int i = 0; i < elementNodes.length; ++i) {
            final Object result = elementNodes[i].executeGeneric(frame);
            assert result instanceof ErlMap.Assoc;
            actualElements[i] = (ErlMap.Assoc) result;
        }

        if (null == baseMap) {
            return ErlMap.fromArray(actualElements);
        }

        return baseMap.makeUpdated(actualElements);
    }

    @Override
    @ExplodeLoop
    public Object match(VirtualFrame frame, Object match) {
        /*
         * This assertion illustrates that the array length is really a constant during compilation.
         */
        CompilerAsserts.compilationConstant(elementNodes.length);

        if (!(match instanceof ErlMap)) {
            return null;
        }

        final ErlMap map = (ErlMap) match;

        for (int i = 0; i < elementNodes.length; ++i) {
            if (null == elementNodes[i].match(frame, map)) {
                return null;
            }
        }

        return map;
    }
}
