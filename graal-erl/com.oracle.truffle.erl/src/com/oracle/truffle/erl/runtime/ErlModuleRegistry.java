package com.oracle.truffle.erl.runtime;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.erl.ErlModule;
import com.oracle.truffle.erl.MFA;
import com.oracle.truffle.erl.nodes.ErlRootNode;

/**
 * Manages the mapping from module names to {@link ErlModuleImpl}.
 */
public final class ErlModuleRegistry {

    private final ErlContext context;
    private final Map<MFA, ErlFunction> BIFs = new HashMap<>();
    private final HashMap<String, ErlModuleImpl> modules = new HashMap<>();

    public ErlModuleRegistry(ErlContext context) {
        this.context = context;
    }

    /**
     * Register a built-in function (BIF).
     *
     * @param moduleName name of the module that the BIF belong
     * @param funcName function name
     * @param arity number of arguments
     * @param rootNode built-in root node
     */
    public synchronized ErlFunction registerBIF(String moduleName, String funcName, int arity, ErlRootNode rootNode) {
        ErlFunction function = new ErlFunction(moduleName, funcName, arity, ErlFunction.Origin.BUILTIN);
        BIFs.put(new MFA(moduleName, funcName, arity), function);
        RootCallTarget callTarget = Truffle.getRuntime().createCallTarget(rootNode);
        function.setCallTarget(callTarget);
        return function;
    }

    /**
     * Register the module in the module registry.
     */
    @TruffleBoundary
    public boolean register(ErlModuleImpl module) {

        synchronized (this) {

            final String moduleName = module.getModuleName();

            // load BIFs into the module
            for (Entry<MFA, ErlFunction> entry : BIFs.entrySet()) {

                final MFA mfa = entry.getKey();

                if (moduleName.equals(mfa.getModule())) {
                    module.register(mfa.getFA(), entry.getValue());
                }
            }

            modules.put(module.getModuleName(), module);
        }

        if (!module.tryRegisterModule(context)) {
            synchronized (this) {
                modules.remove(module.getModuleName());
                return false;
            }
        }

        return true;
    }

    /**
     * Determines whether the module is loaded or not.
     * <p>
     * The function takes care about "real" modules, and the stand-alone BIFs are not considered as
     * module.
     */
    @TruffleBoundary
    public synchronized boolean isModuleLoaded(String module) {
        return modules.containsKey(module);
    }

    /**
     * Find a module in the registry.
     *
     * @param moduleName name of the module
     * @return module or <code>null</code>
     */
    @TruffleBoundary
    public synchronized ErlModule getModule(String moduleName) {
        return modules.get(moduleName);
    }

    @TruffleBoundary
    public void deregister(ErlModuleImpl module) {
        module.onDeregisterModule();
        synchronized (this) {
            modules.remove(module.getModuleName());
        }
    }

    /**
     * Returns the canonical {@link ErlFunction} object for the given name.
     */
    @TruffleBoundary
    public synchronized ErlFunction functionLookup(MFA mfa) {

        // faster branch first: is module loaded?
        final ErlModuleImpl module = modules.get(mfa.getModule());

        // if module is loaded, then just return
        if (null != module) {
            // NOTE: if the function is not found its module, then it will NOT be in the BIFs,
            // because the BIFs are loaded into the corresponding module when it is registered
            return module.lookup(mfa.getFA());
        }

        // slower branch: lookup through the BIFs
        return BIFs.get(mfa);
    }

    /**
     * Same as {@link #functionLookup(MFA)}.
     */
    public synchronized ErlFunction functionLookup(String module, String func, int arity) {
        return functionLookup(new MFA(module, func, arity));
    }

    /**
     * Makes a function reference. The returned function is not necessarily callable.
     */
    public synchronized ErlFunction makeFunction(String module, String func, int arity) {

        final ErlFunction fun = functionLookup(new MFA(module, func, arity));

        if (null != fun) {
            return fun;
        }

        return new ErlFunction(module, func, arity, ErlFunction.Origin.REGULAR);
    }

    /**
     * Returns an unmodifiable set of the pre-loaded module names.
     */
    @TruffleBoundary
    public synchronized Set<String> getPreLoadedModuleNames() {

        HashSet<String> set = new HashSet<>();

        for (ErlModuleImpl module : modules.values()) {
            if (module.isPreLoaded()) {
                set.add(module.getModuleName());
            }
        }

        return Collections.unmodifiableSet(set);
    }

    /**
     * Returns an unmodifiable set of loaded module names.
     */
    @TruffleBoundary
    public synchronized Set<String> getModuleNames() {
        return Collections.unmodifiableSet(modules.keySet());
    }

    /**
     * Returns an unmodifiable set of loaded modules.
     */
    @TruffleBoundary
    public synchronized Collection<ErlModuleImpl> getModules() {
        return Collections.unmodifiableCollection(modules.values());
    }

    /**
     * Returns the sorted list of all functions, for printing purposes only.
     */
    @TruffleBoundary
    public synchronized List<ErlFunction> getFunctions() {

        HashSet<ErlFunction> set = new HashSet<>();

        // accumulate functions in the modules (BIFs are also included)
        for (ErlModuleImpl module : modules.values()) {
            set.addAll(module.getFunctionSet());
        }

        // sort and return
        List<ErlFunction> result = new ArrayList<>(set);
        Collections.sort(result, new Comparator<ErlFunction>() {
            public int compare(ErlFunction lhs, ErlFunction rhs) {
                return lhs.toString().compareTo(rhs.toString());
            }
        });
        return result;
    }
}
