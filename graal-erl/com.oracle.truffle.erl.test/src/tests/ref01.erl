-module(ref01).
-export([main/0, my_ref/0]).

main() ->
	is_reference(my_ref()).

my_ref() ->
	make_ref().
