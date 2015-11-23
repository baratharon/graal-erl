-module(record14).
-export([main/0]).
-record(rec, {field1=default1, field2=default2}).

main() ->
	{ok, #rec.field2, #rec.field1}.
