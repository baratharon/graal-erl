# Welcome to Graal-Erlang!

Graal-Erlang is a Graal-based implementation of the Erlang,
which uses the Truffle and runs on Java 8.


### How does it work?

It works from Erlang source code, and reconstruct the AST inside
the Truffle. Truffle provides many optimization, thus the execution
speed can highly improved.

The internal method is the following:

* Convert the source to Erlang AST using the `erlang/parser.erl`
   * For this, you have to compile the `parser.erl` first to `parser.beam`.
* The AST file is ready to load. This is achived by the ErlAstParser class.
* Ready!


### Get the source

````
$ mkdir work
$ cd work
$ hg clone https://bitbucket.org/allr/mx
$ export PATH=$PWD/mx:$PATH
$ mkdir graal-compiler-workspace
$ cd graal-compiler-workspace
$ mx sclone http://hg.openjdk.java.net/graal/graal-compiler graal
$ git clone https://github.com/baratharon/graal-erl
$ ls -1
graal
graal-erl
jvmci
truffle
````

Optionally, you can use Eclipse. To generate the Eclipse project
files, use the following commands:

````
$ cd graal-erl
$ mx ideinit
````

### Getting up-to-date and build

Instead of using the "raw" Mercurial and git commands, use
`mx` to update the working copy.

````
$ mx spull
````

Use the `mx` for build as well.

````
$ mx build
````

### Running

Basically, use `mx` to run it. However, this will run in interpreted
mode, and no runtime optimizations will occur.

````
$ mx erl
````

To speed up things, add some extra arguments:

````
$ mx --jdk jvmci --dynamicimport graal erl
````

The Graal-Erlang can be used for two different purposes.

#### OTP ring0

OTP ring0 startup.
All arguments are passed to the otp_ring0:start/2 function. The
startup sequence is performed by the OTP ring0. Any arguments can be
used what the "erl" command takes.

````
$ mx erl -init_debug -pa ~/graal-erl/erlang/test -run erlparse03 main "lists:seq(1,100)." -run init stop -noshell
````

#### Independent

The "independent" refers to the independence from the OTP ring0 startup.
This mode will not start any official processes. The result is an empty
Erlang runtime system. Also, modules can be loaded into the runtime system
with the `-file FILE` flag. To select the function to execute, use the
`-mf MODULE FUNCTION` flag. All extra parameters will be passed to the
selected function as a list of strings. That means, the selected function
can be a function with 0 arguments, and with 1 arguments (the list of
strings) as well. To force the usage of the list of strings, use the
`--` flag. After this flag, all remaining arguments are converted to one
list of strings. Also, this can be empty.

````
$ mx erl -independent -file ~/graal-erl/erlang/list41.erl -mf list41 main
````

Without any `-file FILE` flag, the standard input will be used as source.

**Important**: the first line must contain the `module` attribute.

*Note*: the same thing will happen, when the OTP ring0 startup is used.

Example run:

````
$ mx erl -independent -mf tmp main
-module(tmp).

fact(0) ->
    1;   
fact(N) ->
    N * fact(N-1).


main() ->
    fact(5).
````
*(here Ctrl+D was pressed)*
````
120
$
````

### Optional environment variables

* `GRAAL_ERL_PARSER_PATH` sets the path to the `parser.beam`
* `GRAAL_ERL_ROOT_DIR` sets the the Erlang root directory (defaults to `/usr/lib/erlang`)
