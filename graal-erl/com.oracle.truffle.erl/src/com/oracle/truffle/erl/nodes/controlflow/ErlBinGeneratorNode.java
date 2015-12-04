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

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.nodes.ErlExpressionNode;
import com.oracle.truffle.erl.nodes.expression.ErlBinNode;
import com.oracle.truffle.erl.runtime.ErlBinary;
import com.oracle.truffle.erl.runtime.ErlBinaryView;
import com.oracle.truffle.erl.runtime.ErlLazyBinary;

/**
 * TODO
 * <p>
 * The binary generator must be an {@link ErlExpressionNode}, because we have to put into a single
 * array with boolean expressions. Also, parsing this kind of node remains straight forward.
 */
@NodeInfo(shortName = "<=", description = "")
public final class ErlBinGeneratorNode extends ErlExpressionNode {

    @Child private ErlBinNode matchNode;
    @Child private ErlExpressionNode binNode;

    public ErlBinGeneratorNode(SourceSection src, ErlExpressionNode matchNode, ErlExpressionNode binNode) {
        super(src);

        assert matchNode instanceof ErlBinNode;

        this.matchNode = (ErlBinNode) matchNode;
        this.binNode = binNode;

        assert null != matchNode;
        assert null != binNode;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        throw ErlControlException.makeBadarg();
    }

    public static final class GenState {
        private ErlBinary bin;
        private int bitOffset;
        private int bitSize;
        private int[] tmpArray = new int[2];

        private GenState(ErlBinary bin) {
            this.bin = bin;
            this.bitOffset = 0;
            this.bitSize = bin.getBitSize();
        }
    }

    public GenState reset(VirtualFrame frame) {

        final Object result = binNode.executeGeneric(frame);

        if (result instanceof ErlBinary) {

            return new GenState((ErlBinary) result);

        } else if (result instanceof ErlLazyBinary) {

            return new GenState(((ErlLazyBinary) result).construct());

        } else if (result instanceof ErlBinaryView) {

            return new GenState(((ErlBinaryView) result).construct());

        } else {

            throw ErlControlException.makeBadGenerator(result);
        }
    }

    public boolean generate(VirtualFrame frame, GenState genState) {

        if (0 == genState.bitSize) {
            return false;
        }

        try {
            matchNode.match(frame, new ErlBinaryView(genState.bin, genState.bitOffset, genState.bitSize), genState.tmpArray);
            genState.bitOffset += genState.tmpArray[0];
            genState.bitSize -= genState.tmpArray[0];
            return true;
        } catch (ErlControlException ex) {

            if (ex.getSpecialTag() == ErlControlException.SpecialTag.BADMATCH) {

                genState.bitOffset -= genState.bitSize;
                genState.bitSize = 0;
                return false;

            } else {
                throw ex;
            }
        }
    }
}
