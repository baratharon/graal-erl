-module(try06).
-export([main/0]).

main() ->
	try
		begin
			1 = not_one
		end
	catch
		Class:Reason -> {caught, Class, Reason}
	end.
