-module(macro13).
-export([main/0, f/1]).
-define(bla(X), {??X, X}).

f(X) ->
	X * 2 + 1.

main() ->
	?bla(?MODULE:f(3) + 2).
