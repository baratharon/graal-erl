-module(builtin05).
-export([main/0]).

main() ->
	X = an_atom,
	Y = 100,
	Z = 100.1,
	{
		is_number(X),
		is_integer(X),
		is_float(X),
		is_number(Y),
		is_integer(Y),
		is_float(Y),
		is_number(Z),
		is_integer(Z),
		is_float(Z)
	}.
