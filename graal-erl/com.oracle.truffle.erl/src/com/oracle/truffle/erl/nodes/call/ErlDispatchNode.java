/*
 * Copyright (c) 2012, 2015, Oracle and/or its affiliates. All rights reserved.
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
package com.oracle.truffle.erl.nodes.call;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.IndirectCallNode;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlFunction;

public abstract class ErlDispatchNode extends Node {

    protected static final int INLINE_CACHE_SIZE = 2;

    public abstract Object executeDispatch(VirtualFrame frame, ErlFunction function, Object[] arguments);

    @Specialization(guards = "function.getCallTarget() == null")
    protected Object doUndefinedFunction(@SuppressWarnings("unused") ErlFunction function, @SuppressWarnings("unused") Object[] arguments) {
        throw ErlControlException.makeUndef();
    }

    /**
     * Inline cached specialization of the dispatch.
     *
     * <p>
     * Since SL is a quite simple language, the benefit of the inline cache is quite small: after
     * checking that the actual function to be executed is the same as the cachedFuntion, we can
     * safely execute the cached call target. You can reasonably argue that caching the call target
     * is overkill, since we could just retrieve it via {@code function.getCallTarget()}. However,
     * in a more complex language the lookup of the call target is usually much more complicated
     * than in SL. In addition, caching the call target allows method inlining.
     * </p>
     *
     * <p>
     * {@code limit = "INLINE_CACHE_SIZE"} Specifies the limit number of inline cache specialization
     * instantiations.
     * </p>
     * <p>
     * {@code guards = "function == cachedFunction"} The inline cache check. Note that
     * cachedFunction is a final field so that the compiler can optimize the check.
     * </p>
     * <p>
     * {@code assumptions = "cachedFunction.getCallTargetStable()"} Support for function
     * redefinition: When a function is redefined, the call target maintained by the SLFunction
     * object is change. To avoid a check for that, we use an Assumption that is invalidated by the
     * SLFunction when the change is performed. Since checking an assumption is a no-op in compiled
     * code, the assumption check performed by the DSL does not add any overhead during optimized
     * execution.
     * </p>
     *
     * @see Cached
     * @see Specialization
     *
     * @param function the dynamically provided function
     * @param cachedFunction the cached function of the specialization instance
     * @param callNode the {@link DirectCallNode} specifically created for the {@link CallTarget} in
     *            cachedFunction.
     */
    @Specialization(limit = "INLINE_CACHE_SIZE", guards = "function == cachedFunction", assumptions = "cachedFunction.getCallTargetStable()")
    protected static Object doDirect(VirtualFrame frame, ErlFunction function, Object[] arguments,               //
                    @Cached("function") ErlFunction cachedFunction,               //
                    @Cached("create(cachedFunction.getCallTarget())") DirectCallNode callNode) {
        /* Inline cache hit, we are safe to execute the cached call target. */
        Object[] finalArgs = java.util.Arrays.copyOf(arguments, arguments.length + 1);
        finalArgs[arguments.length] = function.getContext();
        return callNode.call(frame, finalArgs);
    }

    /**
     * Slow-path code for a call, used when the polymorphic inline cache exceeded its maximum size
     * specified in <code>INLINE_CACHE_SIZE</code>. Such calls are not optimized any further, e.g.,
     * no method inlining is performed.
     */
    @Specialization(contains = "doDirect")
    protected static Object doIndirect(VirtualFrame frame, ErlFunction function, Object[] arguments,               //
                    @Cached("create()") IndirectCallNode callNode) {
        /*
         * Erlang has a quite simple call lookup: just ask the function for the current call target,
         * and call it.
         */
        Object[] finalArgs = java.util.Arrays.copyOf(arguments, arguments.length + 1);
        finalArgs[arguments.length] = function.getContext();
        return callNode.call(frame, function.getCallTarget(), finalArgs);
    }

}
