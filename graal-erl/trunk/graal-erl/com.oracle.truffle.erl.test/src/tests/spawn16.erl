-module(spawn16).
-export([main/0]).

child() ->
	receive
		Sender -> Sender ! ok
	end.

main() ->
	RegName = my_special_name,
	register(RegName, spawn(fun child/0)),
	RegName ! self(),
	receive
		R -> {result, R}
	after
		100 -> timeout
	end.
