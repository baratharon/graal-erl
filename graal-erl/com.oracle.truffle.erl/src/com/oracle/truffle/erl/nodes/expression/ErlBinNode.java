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
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.nodes.ErlExpressionNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlBinary;
import com.oracle.truffle.erl.runtime.ErlBinaryView;
import com.oracle.truffle.erl.runtime.ErlLazyBinary;

/**
 * Node that constructs a binary or a bit string.
 */
@NodeInfo(shortName = "bin")
public final class ErlBinNode extends ErlExpressionNode {

    @Children private final ErlBinElementNode[] elementNodes;

    public ErlBinNode(SourceSection src, ErlBinElementNode... elementNodes) {
        super(src);
        this.elementNodes = elementNodes;
    }

    @Override
    @ExplodeLoop
    public ErlLazyBinary executeGeneric(VirtualFrame frame) {

        /*
         * This assertion illustrates that the array length is really a constant during compilation.
         */
        CompilerAsserts.compilationConstant(elementNodes.length);

        if (0 == elementNodes.length) {
            return ErlLazyBinary.EMPTY;
        }

        try {

            ErlLazyBinary lazy = new ErlLazyBinary(elementNodes[0].executeBinary(frame));

            for (int i = 1; i < elementNodes.length; ++i) {

                lazy = new ErlLazyBinary(lazy, new ErlLazyBinary(elementNodes[i].executeBinary(frame)));
            }

            return lazy;

        } catch (UnexpectedResultException ex) {

            throw ErlControlException.makeBadarg();
        }
    }

    @Override
    public Object match(VirtualFrame frame, Object match) {

        int[] bitInfo = new int[2];
        final Object result = match(frame, match, bitInfo);

        if (bitInfo[0] == bitInfo[1]) {
            return result;
        }

        return null;
    }

    @ExplodeLoop
    public Object match(VirtualFrame frame, Object match, int[] bitInfo) {
        /*
         * This assertion illustrates that the array length is really a constant during compilation.
         */
        CompilerAsserts.compilationConstant(elementNodes.length);

        Object obj = match;

        if (obj instanceof ErlLazyBinary) {
            obj = ((ErlLazyBinary) obj).construct();
        }

        int bitOffset = 0;
        int bitSize = 0;
        ErlBinary bin;

        if (obj instanceof ErlBinaryView) {

            ErlBinaryView view = (ErlBinaryView) obj;

            bin = view.getViewedBinary();
            bitOffset = view.getBitOffset();
            bitSize = view.getBitSize();

        } else {

            if (!(obj instanceof ErlBinary)) {
                return null;
            }

            bin = (ErlBinary) obj;
            bitSize = bin.getBitSize();
        }

        bitInfo[0] = 0;
        bitInfo[1] = bitSize;

        // System.out.println("MATCH " + bin);

        for (int i = 0; i < elementNodes.length; ++i) {

            final int size = elementNodes[i].match(frame, bin, bitOffset, bitSize);

            if (size < 0) {
                return null;
            }

            bitInfo[0] += size;
            bitOffset += size;
            bitSize -= size;
        }

        return obj;
    }
}
