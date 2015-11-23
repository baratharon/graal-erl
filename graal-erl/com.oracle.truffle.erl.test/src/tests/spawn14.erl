-module(spawn14).
-export([main/0]).

child() ->
	receive
		{Sender, N} -> Sender ! fact(1, N)
	end.

fact(Acc, 0) ->
	Acc;
fact(Acc, N) ->
	receive after 2 -> ok end,
	fact(Acc*N, N-1).

main() ->
	spawn_link(fun child/0) ! {self(), 10000},
	receive
		R -> {result, R}
	after
		1 -> timeout
	end.
