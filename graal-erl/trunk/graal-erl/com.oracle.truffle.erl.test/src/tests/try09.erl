-module(try09).
-export([main/0]).

main() ->
	try
		throw(my_exception)
	catch
		Class:Reason -> {caught, Class, Reason}
	end.
