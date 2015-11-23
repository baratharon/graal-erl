-module(builtin52).
-export([main/0]).

main() ->
	{
		min(1, 1.0),
		min(1.0, 1),
		min(1, 1.1),
		min(1.1, 1),
		min([], {}),
		min([1], #{}),
		min(#{}, #{a=>b}),
		min(#{a=>b}, #{}),
		min(#{a=>a}, #{c=>d}),
		min(#{c=>d}, #{a=>b}),
		min(#{a=>b}, #{a=>c}),
		min(#{a=>c}, #{a=>b}),
		min(12, fun main/0)
	}.
