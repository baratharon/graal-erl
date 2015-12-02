-module(macro12).
-export([main/0]).
-define(MACRO(A), {??A, A}).
-define(X,3).

main() ->
	?MACRO(?X).
