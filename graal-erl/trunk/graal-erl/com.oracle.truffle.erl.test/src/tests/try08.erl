-module(try08).
-export([main/0]).

main() ->
	try
		case there_is_no_case_for_this of
			nope -> ok
		end
	catch
		Class:Reason -> {caught, Class, Reason}
	end.
