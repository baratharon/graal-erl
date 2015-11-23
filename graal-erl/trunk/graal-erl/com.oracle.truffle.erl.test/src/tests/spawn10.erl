-module(spawn10).
-export([main/0]).

child() ->
	receive
		{Sender, find, [], _Fun} -> Sender ! empty_list, child();
		{Sender, find, [H|T], Fun} -> Sender ! {ok, find(H, T, Fun)}, child();
		exit -> ok
	end.

find(Acc, [H|T], Fun) ->
	find(Fun(Acc, H), T, Fun);
find(Acc, [], _Fun) ->
	Acc.

main() ->
	Pid = spawn(fun child/0),
	Pid ! {self(), find, [1,2,3,4,5,6], fun erlang:max/2},
	Pid ! {self(), find, [], fun erlang:min/2},
	Pid ! exit,
	receive R1 -> ok end,
	receive R2 -> ok end,
	{R1, R2}.
