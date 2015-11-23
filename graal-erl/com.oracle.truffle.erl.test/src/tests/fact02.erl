-module(fact02).
-export([fact/1, main/0]).

fact(N) when is_integer(N), N==0 ->
	1;
fact(N) when is_integer(N), N>0 ->
	N * fact(N-1);
fact(_) ->
	bad_argument.

main() ->
	{
		{'1! =',fact(1)},
		{'2! =',fact(2)},
		{'3! =',fact(3)},
		{'5! =',fact(5)},
		{'20! =',fact(20)},
		{'30! =',fact(30)},
		'_____'
	}.
