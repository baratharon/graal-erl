-module(io02).
-export([main/0]).

main() ->
	Term = {number, 10, map, #{}, tuple, {}, list, [], [1,2,3]},
	io:format("Term: ~p~n", [Term]).
