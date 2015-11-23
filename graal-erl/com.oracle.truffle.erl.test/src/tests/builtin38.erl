-module(builtin38).
-export([main/0, myfun/1]).

myfun(X) when is_number(X) ->
	X + 1;
myfun(_X) ->
	huh.

main() ->
	apply(?MODULE, myfun, [37]).
