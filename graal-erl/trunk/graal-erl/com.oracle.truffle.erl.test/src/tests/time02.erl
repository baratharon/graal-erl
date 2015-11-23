-module(time02).
-export([main/0]).

main() ->
	erlang:posixtime_to_universaltime(1443377259).
