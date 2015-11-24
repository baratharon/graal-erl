package com.oracle.truffle.erl.runtime;

import com.oracle.truffle.erl.ErlModule;
import com.oracle.truffle.erl.builtins._module.ModuleInfo1BuiltinFactory;

public class ErlModuleImpl implements ErlModule {

    private final String moduleName;
    private final boolean preLoaded;

    private ErlModuleImpl(final String moduleName, boolean preLoaded) {
        super();
        this.moduleName = moduleName;
        this.preLoaded = preLoaded;
    }

    public static ErlModuleImpl create(final String moduleName, boolean preLoaded) {
        // context.installBuiltin(module, ModuleInfo1BuiltinFactory.getInstance(), true);
        return new ErlModuleImpl(moduleName, preLoaded);
    }

    public String getModuleName() {
        return moduleName;
    }

    public boolean isPreLoaded() {
        return preLoaded;
    }

    public Object call(String functionName, Object... args) {
        // TODO Auto-generated method stub
        return null;
    }
}
