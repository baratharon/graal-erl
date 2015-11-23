-module(builtin49).
-export([main/0]).

main() ->
	{
		try erlang:make_tuple(-1, a) catch error:badarg -> badarg end,
		erlang:make_tuple(0, a),
		erlang:make_tuple(1, a),
		erlang:make_tuple(4, a),
		erlang:make_tuple(4, {}),
		erlang:make_tuple(4, []),
		erlang:make_tuple(4, #{}),
		tuple_size(erlang:make_tuple(1000, a)),
		tuple_size(erlang:make_tuple(2000, [1,2]))
	}.
