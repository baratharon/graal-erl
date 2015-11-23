-module(try05).
-export([main/0]).

main() ->
	try
		1 = my_special_badmatch
	catch
		error:{badmatch, M} -> {badmatch, caught, M}
	end.
