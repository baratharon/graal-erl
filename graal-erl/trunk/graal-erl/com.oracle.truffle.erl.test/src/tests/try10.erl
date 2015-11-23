-module(try10).
-export([main/0]).

main() ->
	try
		exit(my_exit)
	catch
		Class:Reason -> {caught, Class, Reason}
	end.
