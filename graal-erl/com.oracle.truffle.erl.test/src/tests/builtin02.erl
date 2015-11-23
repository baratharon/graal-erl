-module(builtin02).
-export([main/0]).

kind(X) when is_integer(X) ->
	integer;
kind(X) when is_atom(X) ->
	atom;
kind(_) ->
	other.

main() ->
	kind(my_atom).
