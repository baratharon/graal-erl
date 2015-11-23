-module(hash01).
-export([main/0]).

main() ->
	Range = (1 bsl 27) - 1,
	Term1 = [],
	Term2 = {},
	Term3 = #{},
	Term4 = #{a=>b},
	{
		erlang:hash(Term1, Range) /= erlang:hash(Term2, Range),
		erlang:hash(Term2, Range) /= erlang:hash(Term3, Range),
		erlang:hash(Term3, Range) /= erlang:hash(Term4, Range),
		erlang:hash(Term4, Range) /= erlang:hash(Term1, Range),
		erlang:hash(Term1, Range) == erlang:hash([X || X <- []], Range)
	}.
