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

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.ForeignAccess;
import com.oracle.truffle.api.interop.Message;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.erl.ErlangLanguage;
import com.oracle.truffle.erl.nodes.call.ErlDispatchNode;
import com.oracle.truffle.erl.nodes.call.ErlDispatchNodeGen;
import static com.oracle.truffle.erl.runtime.ErlContext.fromForeignValue;
import java.util.List;

/**
 * Implementation of foreign access for {@link ErlFunction}.
 */
final class ErlFunctionForeignAccess implements ForeignAccess.Factory {
    public static ForeignAccess create() {
        return ForeignAccess.create(new ErlFunctionForeignAccess());
    }

    private ErlFunctionForeignAccess() {
    }

    @Override
    public boolean canHandle(TruffleObject o) {
        return o instanceof ErlFunction;
    }

    @Override
    public CallTarget accessMessage(Message tree) {
        if (Message.createExecute(0).equals(tree)) {
            return Truffle.getRuntime().createCallTarget(new ErlForeignCallerRootNode());
        } else if (Message.IS_EXECUTABLE.equals(tree)) {
            return Truffle.getRuntime().createCallTarget(new ErlForeignExecutableCheckNode());
        } else {
            throw new IllegalArgumentException(tree.toString() + " not supported");
        }
    }

    private static class ErlForeignCallerRootNode extends RootNode {
        @Child private ErlDispatchNode dispatch = ErlDispatchNodeGen.create();

        public ErlForeignCallerRootNode() {
            super(ErlangLanguage.class, null, null);
        }

        @Override
        public Object execute(VirtualFrame frame) {
            ErlFunction function = (ErlFunction) ForeignAccess.getReceiver(frame);
            // the calling convention of interop passes the receiver of a
            // function call (the this object)
            // as an implicit 1st argument; we need to ignore this argument for SL
            List<Object> args = ForeignAccess.getArguments(frame);
            Object[] arr;
            if (args.size() > 0 && args.get(0) instanceof ErlContext) {
                arr = args.subList(1, args.size()).toArray();
            } else {
                arr = args.toArray();
            }
            for (int i = 0; i < arr.length; i++) {
                arr[i] = fromForeignValue(arr[i]);
            }
            return dispatch.executeDispatch(frame, function, arr);
        }
    }

    private static class ErlForeignExecutableCheckNode extends RootNode {
        public ErlForeignExecutableCheckNode() {
            super(ErlangLanguage.class, null, null);
        }

        @Override
        public Object execute(VirtualFrame frame) {
            Object receiver = ForeignAccess.getReceiver(frame);
            return receiver instanceof ErlFunction;
        }
    }
}
