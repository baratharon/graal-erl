-module(record11).
-export([main/0]).
-record(rec, {field1, field2}).

func(X) ->
	X.

main() ->
	Rec = #rec{field2=atom},
	{ok, Rec#rec{field1=func([1,2])}}.
