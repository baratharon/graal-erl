-module(builtin51).
-export([main/0]).

main() ->
	{
		max(1, 1.0),
		max(1.0, 1),
		max(1, 1.1),
		max(1.1, 1),
		max([], {}),
		max([1], #{}),
		max(#{}, #{a=>b}),
		max(#{a=>b}, #{}),
		max(#{a=>a}, #{c=>d}),
		max(#{c=>d}, #{a=>b}),
		max(#{a=>b}, #{a=>c}),
		max(#{a=>c}, #{a=>b}),
		max(#{}, fun main/0)
	}.
