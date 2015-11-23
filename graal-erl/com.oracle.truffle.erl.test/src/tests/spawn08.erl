-module(spawn08).
-export([main/0]).

math_service() ->
	math_service(#{}).

math_service(FactCache) ->
	receive
		{Sender, Op='+', X, Y} -> Sender ! {X,Op,Y,'=',X+Y}, math_service(FactCache);
		{Sender, Op='-', X, Y} -> Sender ! {X,Op,Y,'=',X-Y}, math_service(FactCache);
		{Sender, Op='*', X, Y} -> Sender ! {X,Op,Y,'=',X*Y}, math_service(FactCache);
		{Sender, Op='/', X, Y} -> Sender ! {X,Op,Y,'=',X div Y}, math_service(FactCache);
		{Sender, Op='!', N} -> {NewFactCache, R} = fact(FactCache, N), Sender ! {N,Op,'=',R}, math_service(NewFactCache);
		{Sender, get_cache} -> Sender ! {fact_cache, FactCache}, math_service(FactCache);
		{Sender, kill} -> Sender ! die;
		_AnythingElse -> math_service(FactCache)
	end.

fact(FactCache, N) ->
	fact(FactCache, 0, 1, N).

fact(FactCache, Act, Fact, N) ->
	case
		maps:find(N, FactCache)
	of
		error ->
			fact(FactCache#{Act=>Fact}, Act+1, (Act+1)*Fact, N);
		R ->
			{FactCache, R}
	end.

main() ->
	Pid = spawn(fun math_service/0),
	Pid ! {self(), '*', 3, 7},
	Pid ! {self(), '-', 5, 3},
	Pid ! {self(), '/', 7, 2},
	Pid ! {self(), '+', 2, 3},
	Pid ! {self(), '!', 5},
	Pid ! {self(), '!', 6},
	Pid ! {self(), '!', 7},
	Pid ! {self(), get_cache},
	Pid ! {self(), kill},
	receive R1 -> ok end,
	receive R2 -> ok end,
	receive R3 -> ok end,
	receive R4 -> ok end,
	receive R5 -> ok end,
	receive R6 -> ok end,
	receive R7 -> ok end,
	receive R8 -> ok end,
	receive R9 -> ok end,
	{R1, R2, R3, R4, R5, R6, R7, R8, R9}.
