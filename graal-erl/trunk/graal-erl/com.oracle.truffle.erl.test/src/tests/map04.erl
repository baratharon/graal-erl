-module(map04).
-export([main/0]).

the_map() ->
	#{a => 1000, b => 2000, c => 3000}.

main() ->
	M0 = the_map(),
	M1 = M0 # { new => 4000 },
	M2 = M1 # { new := 4001 },
	M2.
