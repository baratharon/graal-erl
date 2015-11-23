-module(case03).
-export([main/0]).

main() ->
	case 1+2==3 of
		true  -> {true};
		false -> {false}
	end.
