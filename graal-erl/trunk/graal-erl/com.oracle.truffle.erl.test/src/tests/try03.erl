-module(try03).
-export([main/0]).

main() ->
	try
		throw(data),
		what_tha
	of
		what_tha -> 'Whyyy?'
	catch
		data -> thrown_data_caught;
		_    -> something_else
	after
		{after_body}
	end.
