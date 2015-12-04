-module(catch03).
-export([main/0]).

main() ->
	{'EXIT', {{badmatch, wont_match}, _}} = (catch 1=wont_match),
	ok.
