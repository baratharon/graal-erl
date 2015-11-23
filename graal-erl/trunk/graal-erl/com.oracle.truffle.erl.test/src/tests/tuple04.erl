-module(tuple04).
-export([main/0]).

main() ->
	{
		tuple_size({}),
		tuple_size({{}}),
		tuple_size({{}, {}}),
		tuple_size({{}, {{},{}}}),
		tuple_size({{{}}}),
		tuple_size({{1,2,3}}),
		tuple_size({[], {}, 1, 5, aaa}),
		tuple_size({1, {2, 3}, 4, {5}, {}}),
		tuple_size({1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20})
	}.
