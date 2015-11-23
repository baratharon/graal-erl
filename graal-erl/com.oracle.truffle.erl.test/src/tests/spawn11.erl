-module(spawn11).
-export([main/0]).

child() ->
	exit(my_exit).

main() ->
	spawn_link(fun child/0),
	receive after
		1000 -> 'what?'
	end.
