-module(builtin44).
-export([main/0]).

main() ->
	{
		try erlang:delete_element(0, {1}) catch error:badarg -> {ok} end,
		try erlang:delete_element(2, {1}) catch error:badarg -> {ok} end,
		try erlang:delete_element(1, {}) catch error:badarg -> {ok} end,
		erlang:delete_element(1, {1,2,3,4,5}),
		erlang:delete_element(2, {1,2,3,4,5}),
		erlang:delete_element(3, {1,2,3,4,5}),
		erlang:delete_element(4, {1,2,3,4,5}),
		erlang:delete_element(5, {1,2,3,4,5})
	}.
