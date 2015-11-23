-module(map02).
-export([main/0]).

the_map() ->
	#{a => 1000, b => 2000, c => 3000}.

main() ->
	the_map().
