package com.oracle.truffle.erl.runtime;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.erl.ErlModule;
import com.oracle.truffle.erl.FA;
import com.oracle.truffle.erl.MFA;
import com.oracle.truffle.erl.builtins.ErlBuiltinNode;
import com.oracle.truffle.erl.builtins._module.ModuleInfo1BuiltinFactory;
import com.oracle.truffle.erl.nodes.ErlRootNode;
import com.oracle.truffle.erl.nodes.controlflow.ErlTailCallException;

public class ErlModuleImpl implements ErlModule {

    private final String moduleName;
    private final boolean preLoaded;
    private final HashMap<FA, ErlFunction> functions = new HashMap<>();
    private final HashSet<FA> onLoadFuntions = new HashSet<>();

    public ErlModuleImpl(final String moduleName, boolean preLoaded) {
        super();
        this.moduleName = moduleName;
        this.preLoaded = preLoaded;

        /**
         * We need to install a built-in that belong to the module. Since it is automatically
         * generated be the Erlang compiler, we implemented as a built-in. However, to make it
         * compatible, we register the function as a {@link ErlFunction.Origin.REGULAR} function.
         */
        final ErlBuiltinNode moduleInfoBIF = ErlContext.makeBuiltin(this, ModuleInfo1BuiltinFactory.getInstance());
        final MFA mfa = moduleInfoBIF.getNames()[0];
        register(mfa.getFunction(), mfa.getArity(), ErlContext.wrapBuiltinBodyNode(moduleInfoBIF, mfa), ErlFunction.Origin.REGULAR);
    }

    public String getModuleName() {
        return moduleName;
    }

    public boolean isPreLoaded() {
        return preLoaded;
    }

    public boolean functionExists(String functionName, int arity) {
        return functions.containsKey(new FA(functionName, arity));
    }

    public Future<Object> start(final ErlContext context, String functionName, Object... args) {

        final ErlFunction func = functions.get(new FA(functionName, args.length));

        if (null != func) {
            final ErlProcess proc = ErlProcess.spawn(context, func, args);

            if (null != proc) {
                return proc.getFuture();
            }
        }

        return null;
    }

    public Object call(final ErlContext context, String functionName, Object... args0) {

        if (null == ErlProcess.getCurrentProcess() || context != ErlProcess.getContext()) {

            Future<Object> future = start(context, functionName, args0);

            if (null != future) {
                try {
                    return future.get();
                } catch (InterruptedException | ExecutionException e) {
                    return null;
                }
            }

            return null;

        } else {

            ErlFunction func = functions.get(new FA(functionName, args0.length));
            Object[] args = args0;

            if (null == func) {
                return null;
            }

            for (;;) {

                try {
                    return func.getCallTarget().call(args);
                } catch (ErlTailCallException tailCallEx) {

                    func = tailCallEx.getFunction();
                    args = tailCallEx.getArguments();
                }
            }
        }
    }

    public void addOnLoadFunction(FA fa) {
        onLoadFuntions.add(fa);
    }

    public boolean tryRegisterModule(ErlContext context) {

        for (FA fa : onLoadFuntions) {

            final ErlFunction func = functions.get(fa);

            if (null == func || 0 != fa.getArity()) {
                return false;
            }

            final Object result = call(context, fa.getFunction());

            if (!ErlAtom.OK.equals(result)) {
                return false;
            }
        }

        return true;
    }

    public ErlFunction lookup(FA fa) {
        return functions.get(fa);
    }

    public Collection<ErlFunction> getFunctionSet() {
        return Collections.unmodifiableCollection(functions.values());
    }

    public ErlFunction register(final FA fa, final ErlFunction func) {
        functions.put(fa, func);
        return func;
    }

    public ErlFunction register(String name, int arity, ErlRootNode rootNode, ErlFunction.Origin origin) {

        final FA fa = new FA(name, arity);

        if (functions.containsKey(fa)) {
            throw new RuntimeException("Redefinition of " + moduleName + ":" + name + "/" + arity);
        }

        ErlFunction function = new ErlFunction(moduleName, name, arity, origin);
        functions.put(fa, function);
        RootCallTarget callTarget = Truffle.getRuntime().createCallTarget(rootNode);
        function.setCallTarget(callTarget);
        return function;
    }
}
