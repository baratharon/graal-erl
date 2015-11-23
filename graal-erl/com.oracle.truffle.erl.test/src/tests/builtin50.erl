-module(builtin50).
-export([main/0]).

main() ->
	{
		try erlang:make_tuple(-1, a, []) catch error:badarg -> badarg end,
		try erlang:make_tuple(1, a, [{1,b}|{1,c}]) catch error:badarg -> badarg end,
		try erlang:make_tuple(1, a, [{0,b}]) catch error:badarg -> badarg end,
		try erlang:make_tuple(1, a, [{2,b}]) catch error:badarg -> badarg end,
		erlang:make_tuple(0, a, []),
		erlang:make_tuple(1, a, [{1,b}]),
		erlang:make_tuple(1, a, [{1,b},{1,c}]),
		erlang:make_tuple(4, a, [{1,b}]),
		erlang:make_tuple(4, {}, [{2,[]}]),
		erlang:make_tuple(4, [], [{2,{}}]),
		erlang:make_tuple(4, #{}, [{3,#{a=>b}}]),
		tuple_size(erlang:make_tuple(1000, a, [{1000,x}])),
		tuple_size(erlang:make_tuple(2000, [1,2], [{1000,x},{2000,y}]))
	}.
