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
package com.oracle.truffle.erl.runtime;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.erl.builtins._module.ModuleInfo1BuiltinFactory;
import com.oracle.truffle.erl.nodes.ErlRootNode;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Manages the mapping from function names to {@link ErlFunction function objects}.
 */
public final class ErlFunctionRegistry {

    private final Map<MFA, ErlFunction> functions = new HashMap<>();
    private final Set<String> loadedModules = new HashSet<>();
    private final Set<String> preLoadedModules = new HashSet<>();

    /**
     * Returns the canonical {@link ErlFunction} object for the given name.
     */
    public synchronized ErlFunction lookup(String module, String func, int arity) {
        return functions.get(new MFA(module, func, arity));
    }

    /**
     * Associates the {@link ErlFunction} with the given name with the given implementation root
     * node.
     */
    public synchronized ErlFunction register(String module, String func, int arity, ErlFunction.Origin origin, ErlRootNode rootNode) {
        final MFA mfa = new MFA(module, func, arity);
        final ErlFunction retFun = functions.get(mfa);

        if (null != retFun) {

            if (ErlFunction.Origin.BUILTIN == retFun.getOrigin() && ErlFunction.Origin.REGULAR == origin) {
                return retFun;
            }

            throw new RuntimeException("Redefinition of " + module + ":" + func + "/" + arity);
        }

        ErlFunction function = new ErlFunction(module, func, arity, origin);
        functions.put(mfa, function);
        RootCallTarget callTarget = Truffle.getRuntime().createCallTarget(rootNode);
        function.setCallTarget(callTarget);
        return function;
    }

    /**
     * Returns the sorted list of all functions, for printing purposes only.
     */
    public synchronized List<ErlFunction> getFunctions() {
        List<ErlFunction> result = new ArrayList<>(functions.values());
        Collections.sort(result, new Comparator<ErlFunction>() {
            public int compare(ErlFunction f1, ErlFunction f2) {
                return f1.toString().compareTo(f2.toString());
            }
        });
        return result;
    }

    public synchronized void addLoadedModule(ErlContext context, String module, final boolean preLoaded) {
        context.installBuiltin(module, ModuleInfo1BuiltinFactory.getInstance(), true);
        loadedModules.add(module);

        if (preLoaded) {
            preLoadedModules.add(module);
        }
    }

    public synchronized boolean isModuleLoaded(String module) {
        return loadedModules.contains(module);
    }

    /**
     * Returns an unmodifiable set of the pre-loaded modules.
     */
    public synchronized Set<String> getPreLoaded() {
        return Collections.unmodifiableSet(preLoadedModules);
    }

    /**
     * Returns an unmodifiable set of loaded modules.
     */
    public synchronized Set<String> getModules() {
        return Collections.unmodifiableSet(loadedModules);
    }
}
