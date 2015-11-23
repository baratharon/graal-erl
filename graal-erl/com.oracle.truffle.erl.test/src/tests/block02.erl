-module(block02).
-export([main/0]).

main() ->
	begin
		X = begin Y=1, Y end
	end,
	begin
		X = begin Y=X, Y end
	end.
