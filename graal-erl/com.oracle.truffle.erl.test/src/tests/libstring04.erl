-module(libstring04).
-export([main/0]).

main() ->
	string:tokens("abc defxxghix jkl", "x ").
