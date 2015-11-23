-module(spawn15).
-export([main/0]).

child() ->
	receive
		{Sender, N} -> Sender ! fact(1, N)
	end.

fact(Acc, 0) ->
	Acc;
fact(Acc, N) ->
	fact(Acc*N, N-1).

main() ->
	spawn_link(fun child/0) ! {self(), 30},
	receive
		R -> {result, R}
	after
		1000 -> timeout
	end.
