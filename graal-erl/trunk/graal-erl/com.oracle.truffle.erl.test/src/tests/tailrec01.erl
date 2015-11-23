-module(tailrec01).
-export([main/0, re/1]).

re(0) ->
	ok;
re(N) ->
	re(N-1).

main() ->
	re(5000).
