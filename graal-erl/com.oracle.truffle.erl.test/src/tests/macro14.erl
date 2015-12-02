-module(macro14).
-export([main/0, f/1]).
-define(bla(X), {??X, X}).

f(X) ->
	X * 2 + 1.

main() ->
	?bla({[], #{}, <<1,2,3>>, [1,2,3]}).
