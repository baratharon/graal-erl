-module(macro01).
-export([main/0]).
-define(THE_ATOM, my_atom).

main() ->
	?THE_ATOM.
