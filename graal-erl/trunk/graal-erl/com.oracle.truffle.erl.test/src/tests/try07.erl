-module(try07).
-export([main/0]).

main() ->
	try
		if
			false -> ok
		end
	catch
		Class:Reason -> {caught, Class, Reason}
	end.
