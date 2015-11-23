-module(builtin45).
-export([main/0]).

main() ->
	{
		try erlang:insert_element(0, {1}, #{}) catch error:badarg -> {ok} end,
		try erlang:insert_element(3, {1}, #{}) catch error:badarg -> {ok} end,
		try erlang:insert_element(2, {}, #{}) catch error:badarg -> {ok} end,
		erlang:insert_element(1, {1,2,3,4,5}, #{}),
		erlang:insert_element(2, {1,2,3,4,5}, #{}),
		erlang:insert_element(3, {1,2,3,4,5}, #{}),
		erlang:insert_element(4, {1,2,3,4,5}, #{}),
		erlang:insert_element(5, {1,2,3,4,5}, #{}),
		erlang:insert_element(6, {1,2,3,4,5}, #{})
	}.
