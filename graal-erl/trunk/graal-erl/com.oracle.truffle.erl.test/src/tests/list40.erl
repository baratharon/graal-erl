-module(list40).
-export([main/0]).

flush() ->
	receive
		_ -> flush()
	after
		0 -> ok
	end.

main() ->
	flush(),
	self() ! 2,
	self() ! 1,
	self() ! 3,
	[ receive M -> M end || M <- [1, 2, 3] ].
