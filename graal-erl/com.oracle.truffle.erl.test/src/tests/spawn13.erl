-module(spawn13).
-export([main/0, child/1]).

child(0) ->
	normal_exit;
child(N) ->
	spawn_link(?MODULE, child, [N-1]),
	receive after
		200 -> {'what?', N}
	end.

main() ->
	spawn_link(?MODULE, child, [3]),
	receive after
		200 -> normal_exit
	end.
