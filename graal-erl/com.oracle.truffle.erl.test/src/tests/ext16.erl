-module(ext16).
-export([main/0]).

main() ->
	term_to_binary(#{1=>2, 2=>3, 3=>4}).
