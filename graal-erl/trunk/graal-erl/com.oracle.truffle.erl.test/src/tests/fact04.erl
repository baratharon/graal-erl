-module(fact04).
-export([fact/1, main/0]).

fact(N) when is_integer(N), N>=0 ->
	fact_impl(1, N);
fact(_) ->
	bad_argument.

fact_impl(Acc, 0) ->
	Acc;
fact_impl(Acc, N) ->
	fact_impl(Acc*N, N-1).

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

main2() ->
	{
		{'1! =', fact(1)},
		{'2! =', fact(2)},
		{'3! =', fact(3)},
		{'4! =', fact(4)},
		{'5! =', fact(5)},
		{'10! =', fact(10)},
		{'20! =', fact(20)},
		{'30! =', fact(30)},
		{'40! =', fact(40)},
		{'50! =', fact(50)},
		{'60! =', fact(60)},
		{'80! =', fact(80)},
		{'100! =', fact(100)},
		{'150! =', fact(150)},
		{'200! =', fact(200)},
		{'500! =', fact(500)},
		{'1000! =', fact(1000)},
		'_____'
	}.
