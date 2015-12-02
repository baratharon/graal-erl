-module(anonymous13).
-export([main/0]).

eval(Fun, A, B, Info, Eval) ->
	{Info, Fun(A, B), Eval}.

mkfun(Op) ->
	Info = {op, Op},
	F =
	case Op of
		'+' -> fun Eval(A, B) -> eval(fun erlang:'+'/2,   A, B, Info, Eval) end;
		'-' -> fun Eval(A, B) -> eval(fun erlang:'-'/2,   A, B, Info, Eval) end;
		'*' -> fun Eval(A, B) -> eval(fun erlang:'*'/2,   A, B, Info, Eval) end;
		'/' -> fun Eval(A, B) -> eval(fun erlang:'div'/2, A, B, Info, Eval) end
	end,
	F.

main()->
	F1 = mkfun('+'),
	F2 = mkfun('*'),
	F3 = mkfun('/'),
	R1 = {_, _, G1} = F1(3, 4),
	R2 = {_, _, G2} = F2(2, 3),
	R3 = {_, _, G3} = F3(16, 2),
	{
		erlang:delete_element(3, R1),
		erlang:delete_element(3, R2),
		erlang:delete_element(3, R3),
		erlang:delete_element(3, G1(3, 4)),
		erlang:delete_element(3, G2(2, 3)),
		erlang:delete_element(3, G3(16, 2))
	}.
