Example how to run the benchmark:

````
time mx -v --jdk jvmci --dynamicimport graal erl -run binarytrees main 20 -run shell stop -noshell
````

And with debugging information:

````
time mx -v --jdk jvmci --dynamicimport graal erl -G:+TraceTruffleCompilation -G:+TruffleSplitting -G:+TruffleCompilationExceptionsAreFatal -Xprof -ea -esa -run binarytrees main 20 -run shell stop -noshell
````
