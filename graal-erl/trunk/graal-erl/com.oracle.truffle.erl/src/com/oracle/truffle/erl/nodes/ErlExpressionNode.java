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
package com.oracle.truffle.erl.nodes;

import com.oracle.truffle.api.dsl.TypeSystemReference;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlBinary;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlFunction;
import com.oracle.truffle.erl.runtime.ErlLazyBinary;
import com.oracle.truffle.erl.runtime.ErlMap;
import com.oracle.truffle.erl.runtime.ErlTuple;

import java.io.File;
import java.math.BigInteger;

/**
 * The base class of all Truffle nodes for Erlang. All nodes (even expressions) can be used as
 * statements, i.e., without returning a value. The {@link VirtualFrame} provides access to the
 * local variables.
 */
@TypeSystemReference(ErlTypes.class)
@NodeInfo(language = "Simple Language", description = "The abstract base node for all expressions")
public abstract class ErlExpressionNode extends Node {

    public ErlExpressionNode(SourceSection src) {
        super(src);
    }

    /**
     * Execute this node as an expression (form). All forms have return value, so this function must
     * provide it.
     */
    public abstract Object executeGeneric(VirtualFrame frame);

    /**
     * Execute the node, and match the value. Data constructor nodes shall override this method.
     *
     * @param frame Same as in executeGeneric(VirtualFrame).
     * @param match The value to match.
     * @return The return value is the same as executeGeneric(VirtualFrame).
     */
    public Object match(VirtualFrame frame, Object match) {
        final Object result = executeGeneric(frame);
        if (0 != ErlContext.compareTerms(result, match, true)) {
            throw ErlControlException.makeBadmatch(match);
        }
        return result;
    }

    /**
     * Mark the node as a tail node. The purpose of this node is to be able to mark invocations as
     * tail-call. Tail-calls are most stack efficient, and provides "unlimited" depth for e.g. event
     * loop functions.
     * <p>
     * For numerous expressions, marking something as tail does not make any difference, so
     * providing a default implementation is handy.
     */
    public void markAsTail() {
        // empty
    }

    /*
     * Execute methods for specialized types. They all follow the same pattern: they call the
     * generic execution method and then expect a result of their return type. Type-specialized
     * subclasses overwrite the appropriate methods.
     */

    public long executeLong(VirtualFrame frame) throws UnexpectedResultException {
        return ErlTypesGen.expectLong(executeGeneric(frame));
    }

    public ErlAtom executeAtom(VirtualFrame frame) throws UnexpectedResultException {
        return ErlTypesGen.expectErlAtom(executeGeneric(frame));
    }

    public BigInteger executeBigInteger(VirtualFrame frame) throws UnexpectedResultException {
        return ErlTypesGen.expectBigInteger(executeGeneric(frame));
    }

    public double executeDouble(VirtualFrame frame) throws UnexpectedResultException {
        return ErlTypesGen.expectDouble(executeGeneric(frame));
    }

    public ErlFunction executeFunction(VirtualFrame frame) throws UnexpectedResultException {
        return ErlTypesGen.expectErlFunction(executeGeneric(frame));
    }

    public ErlMap executeMap(VirtualFrame frame) throws UnexpectedResultException {
        return ErlTypesGen.expectErlMap(executeGeneric(frame));
    }

    public ErlTuple executeTuple(VirtualFrame frame) throws UnexpectedResultException {
        return ErlTypesGen.expectErlTuple(executeGeneric(frame));
    }

    public ErlBinary executeBinary(VirtualFrame frame) throws UnexpectedResultException {
        return ErlTypesGen.expectErlBinary(executeGeneric(frame));
    }

    public ErlLazyBinary executeLazyBinary(VirtualFrame frame) throws UnexpectedResultException {
        return ErlTypesGen.expectErlLazyBinary(executeGeneric(frame));
    }

    public boolean executeBoolean(VirtualFrame frame) throws UnexpectedResultException {
        return ErlTypesGen.expectBoolean(executeGeneric(frame));
    }

    public ErlExpressionNode getNonWrapperNode() {
        return this;
    }

    @Override
    public String toString() {
        return formatSourceSection(this);
    }

    /**
     * Formats a source section of a node in human readable form. If no source section could be
     * found it looks up the parent hierarchy until it finds a source section. Nodes where this was
     * required append a <code>'~'</code> at the end.
     *
     * @param node the node to format.
     * @return a formatted source section string
     */
    public static String formatSourceSection(Node node) {
        if (node == null) {
            return "<unknown>";
        }
        SourceSection section = node.getSourceSection();
        boolean estimated = false;
        if (section == null) {
            section = node.getEncapsulatingSourceSection();
            estimated = true;
        }

        if (section == null || section.getSource() == null) {
            return "<unknown source>";
        } else {
            String sourceName = new File(section.getSource().getName()).getName();
            int startLine = section.getStartLine();
            return String.format("%s:%d%s", sourceName, startLine, estimated ? "~" : "");
        }
    }
}
