-module(call09).
-export([main/0, my_apply/2]).

my_apply(Fun, {}) ->
	Fun();
my_apply(Fun, {Arg1}) ->
	Fun(Arg1);
my_apply(Fun, {Arg1, Arg2}) ->
	Fun(Arg1, Arg2);
my_apply(Fun, {Arg1, Arg2, Arg3}) ->
	Fun(Arg1, Arg2, Arg3);
my_apply(Fun, {Arg1, Arg2, Arg3, Arg4}) ->
	Fun(Arg1, Arg2, Arg3, Arg4);
my_apply(Fun, Args) ->
	{too_many_arguments, {function, Fun}, {args, Args}}.

main()->
	Fun1 = fun(N) -> is_integer(N) end,
	Fun2 = fun(N) -> Fun1(N) end,
	my_apply(Fun2, {10+20}) xor my_apply(Fun2, {2==3}).
