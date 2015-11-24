package com.oracle.truffle.erl;

public interface ErlModule {

    public String getModuleName();

    public boolean isPreLoaded();

    public Object call(final String functionName, Object... args);
}
