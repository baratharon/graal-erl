-module(string04).
-export([main/0]).

s("Hello " ++ Rest) ->
	{ok, Rest};
s(X) ->
	{no, {X}}.

main() ->
	s("Hello World!").
