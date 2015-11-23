-module(list39).
-export([main/0]).

main() ->
	[ Map#{new => X} || X <- [1,2,3], Map <- [#{}, #{old => 1}] ].
