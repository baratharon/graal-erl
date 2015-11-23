-module(atom05).
-export([main/0]).

main() ->
	{
		atom_to_binary(atom, latin1),
		atom_to_binary(long_long_long_long_atom, latin1),
		atom_to_binary('Fancy atom %&#\xAA\xDD', latin1),
		atom_to_binary(atom, utf8),
		atom_to_binary(long_long_long_long_atom, utf8),
		bit_size(atom_to_binary('Fancy atom %&#\xAA\xDD', utf8))
	}.
