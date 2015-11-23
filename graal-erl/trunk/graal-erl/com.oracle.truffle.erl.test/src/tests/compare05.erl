-module(compare05).
-export([main/0]).

main() ->
	{
		#{} == #{},
		#{} /= #{},
		#{} =:= #{},
		#{} =/= #{}
	}.
