-module(ext17).
-export([main/0]).

main() ->
	term_to_binary(#{[]=>[1|2], {}=>#{}}).
