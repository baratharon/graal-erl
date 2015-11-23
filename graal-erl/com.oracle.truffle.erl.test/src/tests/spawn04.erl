-module(spawn04).
-export([main/0]).

main() ->
	Timo = 10,
	receive
	after
		Timo -> {ok, delayed, Timo, msec}
	end.
