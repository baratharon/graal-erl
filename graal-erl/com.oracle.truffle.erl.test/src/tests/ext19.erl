-module(ext19).
-export([main/0]).

main() ->
	term_to_binary(3.14, [{minor_version, 0}]).
