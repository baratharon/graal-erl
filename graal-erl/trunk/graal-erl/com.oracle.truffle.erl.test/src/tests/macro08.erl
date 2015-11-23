-module(macro08).
-export([main/0]).
-define(MUL(A,B), A*B).

main() ->
	?MUL(1+2, 3+4).
