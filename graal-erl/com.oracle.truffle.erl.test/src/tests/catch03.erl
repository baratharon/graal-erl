-module(catch03).
-export([main/0]).

main() ->
	catch 1=wont_match.
