-module(string07).
-export([main/0]).

main() ->
	[[C] || C <- lists:seq(0, 256)].
