-module(tuple02).
-export([main/0]).

make_tuple() ->
	{this, is, 'a', tuple, '=', {5,'band',3,'bxor',7}}.

main() ->
	{'Module ', ?MODULE, ' says :', make_tuple(), 0, make_tuple(), 0, 'end'}.
