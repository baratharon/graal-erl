-module(time01).
-export([main/0]).

main() ->
	erlang:universaltime_to_posixtime({{2015,9,27},{18,7,39}}).
