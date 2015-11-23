-module(tuple09).
-export([main/0]).

main() ->
	{
		try erlang:setelement(0, {1}, atom) catch error:badarg -> ok end,
		try erlang:setelement(2, {1}, atom) catch error:badarg -> ok end,
		try erlang:setelement(x, {1}, atom) catch error:badarg -> ok end,
		try erlang:setelement(1, not_tuple, atom) catch error:badarg -> ok end,
		erlang:setelement(1, {1}, atom),
		erlang:setelement(2, {1,2,3}, atom),
		erlang:setelement(3, {a,b,c}, #{}),
		size(erlang:setelement(4, {1,2,3,4,5,6,7,8}, atom))
	}.
