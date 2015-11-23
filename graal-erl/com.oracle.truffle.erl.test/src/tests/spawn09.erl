-module(spawn09).
-export([main/0, child/3]).

child(Name, Val, Parent) ->
	receive
		{Acc, Names} -> Parent ! {Acc+Val, [Name|Names]};
		start -> Parent ! {Val, [Name]}
	end.

main() ->
	Pid1 = spawn(?MODULE, child, [p1, 2, self()]),
	Pid2 = spawn(?MODULE, child, [p2, 4, Pid1]),
	Pid3 = spawn(?MODULE, child, [p3, 3, Pid2]),
	Pid4 = spawn(?MODULE, child, [p4, 5, Pid3]),
	Pid4 ! start,
	receive
		{Result, Names} -> {yay, Result, Names}
	end.
