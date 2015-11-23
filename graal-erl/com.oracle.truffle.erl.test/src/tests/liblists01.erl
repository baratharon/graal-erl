-module(liblists01).
-export([main/0]).

main() ->
	lists:flatten([1,[2,[3,[4,5],[6,7],8],9,10],11]).
