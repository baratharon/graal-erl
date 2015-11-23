-module(try01).
-export([main/0]).

main() ->
	try
		throw(data),
		what_tha
	catch
		data -> thrown_data_caught;
		_    -> something_else
	end.
