-module(record04).
-export([main/0]).
-record(rec, {field1, field2}).

main() ->
	Rec = #rec{field2=atom},
	Rec#rec{field1=1234}.
