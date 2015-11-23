-module(exb18).
-export([main/0]).

main() ->
	binary_to_term(<<131,70,64,9,30,184,81,235,133,31>>).
