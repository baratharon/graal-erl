-module(catch02).
-export([main/0]).

main() ->
	catch throw(my_exception).
