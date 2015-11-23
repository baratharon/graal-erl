-module(builtin37).
-export([main/0]).

main() ->
	Fun = fun(X) when is_number(X) -> X + 1; (_X) -> huh end,
	apply(Fun, [37]).
