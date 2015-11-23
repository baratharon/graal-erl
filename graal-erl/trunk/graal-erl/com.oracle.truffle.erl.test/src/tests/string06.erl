-module(string06).
-export([main/0]).

s("Hello" ++ [X | Rest]) ->
	{ok, X, Rest};
s(X) ->
	{no, {X}}.

main() ->
	s("Hello World!").
