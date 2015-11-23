-module(try04).
-export([main/0]).

main() ->
	try
		erlang:error({badmatch, '?'}),
		what_tha
	catch
		error:{badmatch, M} -> {badmatch, caught, M}
	end.
