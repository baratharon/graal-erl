-module(builtin36).
-export([main/0]).

main() ->
	T1 = {},
	T2 = erlang:append_element(T1, 1),
	T3 = erlang:append_element(T2, 2),
	T4 = erlang:append_element(T3, 3),
	T5 = erlang:append_element(T4, 4),
	T6 = erlang:append_element(T5, 5),
	T6.
