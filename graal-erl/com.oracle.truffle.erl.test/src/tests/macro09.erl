-module(macro09).
-export([main/0]).
-define(GUARD(A), 10=<A, A=<19).

f(X) when ?GUARD(X) ->
	true;
f(_) ->
	false.

main() ->
	{
		f(9),
		f(10),
		f(15),
		f(19),
		f(20)
	}.
