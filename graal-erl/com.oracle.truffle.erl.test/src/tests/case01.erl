-module(case01).
-export([main/0]).

main() ->
	case 1+2+3 of
		5 -> {wrong, 5};
		6 -> {correct, 6};
		7 -> {wrong, 7}
	end.
