-module(map03).
-export([main/0]).

the_map() ->
	#{a => 1000, b => 2000, c => 3000}.

main() ->
	M0 = the_map(),
	M0 # { new => 4000 }.
