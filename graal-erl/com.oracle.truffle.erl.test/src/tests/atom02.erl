-module(atom02).
-export([main/0, atomfunc/0]).

atomfunc() ->
	my_atom.

main() ->
	atomfunc().
