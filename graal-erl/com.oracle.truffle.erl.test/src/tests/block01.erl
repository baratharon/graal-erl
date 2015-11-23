-module(block01).
-export([main/0]).

main() ->
	begin
		X = 1,
		Y = 2,
		X = X,
		X + Y
	end.
