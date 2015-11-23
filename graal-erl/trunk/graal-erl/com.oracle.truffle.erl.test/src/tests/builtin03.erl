-module(builtin03).
-export([main/0]).

main() ->
	REF = make_ref(),
	ATOM = atom,
	LIST = [1],
	{
		is_reference(REF),
		is_atom(REF),
		is_list(REF),
		is_reference(ATOM),
		is_atom(ATOM),
		is_list(ATOM),
		is_reference(LIST),
		is_atom(LIST),
		is_list(LIST),
		'-- end --'
	}.
