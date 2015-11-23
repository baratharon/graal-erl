-module(match18).
-export([main/0]).

f(A, A) ->
	A;
f(A, B) when is_integer(A), is_integer(B) ->
	A*B;
f(_, _) ->
	other.

main() ->
	{
		f(1, 2.0),
		f(ok, ok),
		f(2, 3),
		f(4, 4),
		f(5, ok)
	}.
