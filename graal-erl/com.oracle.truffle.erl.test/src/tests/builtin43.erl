-module(builtin43).
-export([main/0]).

main() ->
	DT1 = {erlang:date(), erlang:time()},
	DT2 = {erlang:date(), erlang:time()},
	DT1 =< DT2.
