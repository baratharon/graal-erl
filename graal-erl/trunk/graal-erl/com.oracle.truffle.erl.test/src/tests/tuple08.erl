-module(tuple08).
-export([main/0]).

main() ->
	{
		try size(not_a_tuple) catch error:badarg -> {ok} end,
		size({}),
		size({1}),
		size({{}}),
		size({[<<>>]}),
		size({1,2,3,4}),
		size(erlang:append_element({1,2,3,4},5))
	}.
