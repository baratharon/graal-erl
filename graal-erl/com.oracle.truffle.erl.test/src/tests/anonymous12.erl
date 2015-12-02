-module(anonymous12).
-export([main/0]).

eval(Fun, A, B, Info, _Eval) ->
	{Info, Fun(A, B)}.

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
	{ F1(3, 4), F2(2, 3), F3(16, 2) }.
