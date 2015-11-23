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
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.nodes.ErlExpressionNode;
import com.oracle.truffle.erl.runtime.ErlList;

/**
 * TODO
 */
@NodeInfo(shortName = "[||]", description = "")
public class ErlListComprehensionNode extends ErlExpressionNode {

    @Child protected ErlExpressionNode exprNode;
    @Children protected final ErlExpressionNode[] qualifierNodes;

    public ErlListComprehensionNode(SourceSection src, ErlExpressionNode exprNode, ErlExpressionNode[] qualifierNodes) {
        super(src);
        this.exprNode = exprNode;
        this.qualifierNodes = qualifierNodes;

        assert null != exprNode;
        assert null != qualifierNodes && 0 < qualifierNodes.length;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {

        List<Object> elements = new ArrayList<>();
        build(frame, elements, 0);
        return ErlList.fromList(elements);
    }

    protected void build(VirtualFrame frame, List<Object> elements, int firstQualifier) {

        if (firstQualifier < qualifierNodes.length) {

            final ErlExpressionNode qual = qualifierNodes[firstQualifier];

            if (qual instanceof ErlListGeneratorNode) {

                ErlListGeneratorNode generator = (ErlListGeneratorNode) qual;

                ErlListGeneratorNode.GenState genState = generator.reset(frame);
                while (generator.generate(frame, genState)) {
                    build(frame, elements, firstQualifier + 1);
                }

            } else if (qual instanceof ErlBinGeneratorNode) {

                ErlBinGeneratorNode generator = (ErlBinGeneratorNode) qual;

                ErlBinGeneratorNode.GenState genState = generator.reset(frame);
                while (generator.generate(frame, genState)) {
                    build(frame, elements, firstQualifier + 1);
                }

            } else {

                try {
                    if (qual.executeBoolean(frame)) {
                        build(frame, elements, firstQualifier + 1);
                    }
                } catch (UnexpectedResultException e) {

                    // we reevaluate the filter, but this is executed only in extreme situations
                    throw ErlControlException.makeBadFilter(qual.executeGeneric(frame));
                }
            }
        } else {

            elements.add(exprNode.executeGeneric(frame));
        }
    }
}
