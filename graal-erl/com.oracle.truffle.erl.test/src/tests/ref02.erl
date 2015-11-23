-module(ref02).
-export([main/0]).

main() ->
	R1 = make_ref(),
	R2 = make_ref(),
	{result,
		{'==', R1==R2},
		{'/=', R1/=R2},
		{'=:=', R1=:=R2},
		{'=/=', R1=/=R2},
		{'end'}
	}.
