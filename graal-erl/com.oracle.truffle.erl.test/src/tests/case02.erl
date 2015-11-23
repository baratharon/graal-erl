-module(case02).
-export([main/0]).

f_case(N) ->
	case N of
		N when is_number(N) -> {number, N};
		N when is_atom(N) -> {atom, N};
		N when is_list(N) -> {list, N};
		N when is_tuple(N) -> {tuple, N};
		Stg -> {something_else, Stg}
	end.

my_map(_Mapper, []) ->
	[];
my_map(Mapper, List) ->
	my_map_impl(Mapper, List, []).

my_map_impl(_Mapper, [], Acc) ->
	my_rev(Acc);
my_map_impl(Mapper, [H|T], Acc) ->
	my_map_impl(Mapper, T, [Mapper(H) | Acc]).

my_rev([]) ->
	[];
my_rev(List) ->
	my_rev_impl([], List).

my_rev_impl(Acc, []) ->
	Acc;
my_rev_impl(Acc, [H|T]) ->
	my_rev_impl([H | Acc], T).

main() ->
	Fun = fun
		(X) when is_integer(X) -> X+1;
		(X) when is_float(X) -> X*X;
		(X) -> X
	end,
	my_map(fun(X) -> f_case(Fun(X)) end, [3, 1.5, hello, [], {}]).
