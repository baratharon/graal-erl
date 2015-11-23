-module(call08).
-export([main/0, my_call/4, my_apply/2]).

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

my_call(Module, Func, Arity, ArgTuple) ->
	my_apply(fun Module:Func/Arity, ArgTuple).

call_wrap(Module, Func, Arity, ArgTuple) ->
	my_call(?MODULE, my_call, 4, {Module, Func, Arity, ArgTuple}).

main()->
	call_wrap(erlang, is_integer, 1, {10+20}) xor call_wrap(erlang, is_integer, 1, {2==3}).
