-module(builtin47).
-export([main/0, myfunc/0, myfunc/1]).

myfunc() ->
	yeah.

myfunc(_) ->
	yeah.

main() ->
	{
		erlang:is_builtin(?MODULE, main, -1),
		erlang:is_builtin(?MODULE, main, 0),
		erlang:is_builtin(?MODULE, main, 1),
		erlang:is_builtin(?MODULE, main, 2),
		erlang:is_builtin(?MODULE, myfunc, -1),
		erlang:is_builtin(?MODULE, myfunc, 0),
		erlang:is_builtin(?MODULE, myfunc, 1),
		erlang:is_builtin(?MODULE, myfunc, 2),
		erlang:is_builtin(erlang, is_builtin, -1),
		erlang:is_builtin(erlang, is_builtin, 0),
		erlang:is_builtin(erlang, is_builtin, 3),
		erlang:is_builtin(erlang, is_builtin, 4)
	}.
