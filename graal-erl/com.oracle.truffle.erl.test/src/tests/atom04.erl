-module(atom04).
-export([main/0]).

main() ->
	{
		atom_to_list(atom),
		atom_to_list(long_long_long_long_atom),
		atom_to_list('Fancy atom %&#')
	}.
