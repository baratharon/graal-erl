-module(if01).
-export([main/0]).

f(X, Y) ->
	X = X,
	if
		X < Y ->
			true;
		true ->
			false
	end.

main() ->
	not f(3, 2).
