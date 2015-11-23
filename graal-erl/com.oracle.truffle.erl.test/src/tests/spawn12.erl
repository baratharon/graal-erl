-module(spawn12).
-export([main/0, child/1]).

child(0) ->
	exit(my_exit);
child(N) ->
	spawn_link(?MODULE, child, [N-1]),
	receive after
		1000 -> {'what?', N}
	end.

main() ->
	spawn_link(?MODULE, child, [3]),
	receive after
		1000 -> 'what?'
	end.
