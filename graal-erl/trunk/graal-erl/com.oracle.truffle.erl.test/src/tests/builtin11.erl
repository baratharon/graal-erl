-module(builtin11).
-export([main/0]).

main() ->
	Map = #{},
	Tuple = {},
	List = [],
	{
		is_map(Map),
		is_tuple(Map),
		is_list(Map),
		is_map(Tuple),
		is_tuple(Tuple),
		is_list(Tuple),
		is_map(List),
		is_tuple(List),
		is_list(List)
	}.
