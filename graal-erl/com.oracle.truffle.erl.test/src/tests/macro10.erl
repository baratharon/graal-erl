-module(macro10).
-export([main/0]).
-define(A_MACRO, 1).

-ifdef(A_MACRO).
	-ifdef(A_MACRO).
		f() -> 1.
	-else.
		f() -> 2.
	-endif.
-else.
	-ifdef(A_MACRO).
		f() -> 3.
	-else.
		f() -> 4.
	-endif.
-endif.

-ifndef(NO_SUCH_MACRO).
	-ifndef(NO_SUCH_MACRO).
		g() -> 10.
	-else.
		g() -> 20.
	-endif.
-else.
	-ifndef(NO_SUCH_MACRO).
		g() -> 30.
	-else.
		g() -> 40.
	-endif.
-endif.

main() ->
	f() + g().
