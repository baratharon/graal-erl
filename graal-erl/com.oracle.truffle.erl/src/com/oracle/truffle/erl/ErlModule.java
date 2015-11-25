package com.oracle.truffle.erl;

import java.util.concurrent.Future;

import com.oracle.truffle.erl.runtime.ErlContext;

public interface ErlModule {

    public String getModuleName();

    public boolean isPreLoaded();

    public Future<Object> start(final ErlContext context, final String functionName, Object... args);

    public Object call(final ErlContext context, final String functionName, Object... args);
}
