-module(list36).
-export([main/0]).

main() ->
	Bin = <<11:7, 22:6, 33:5, 44:4, 55:3>>,
	[ X || <<X:5>> <= Bin].
