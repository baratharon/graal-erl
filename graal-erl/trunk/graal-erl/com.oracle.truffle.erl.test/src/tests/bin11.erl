-module(bin11).
-export([main/0]).

main() ->
	try
		<<10/binary>>
	catch
		error:badarg -> {ok, badarg}
	end.
