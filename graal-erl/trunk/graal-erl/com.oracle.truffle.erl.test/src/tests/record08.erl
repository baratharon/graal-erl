-module(record08).
-export([main/0]).
-record(rec, {field1, field2}).

main() ->
	{#rec{field1=123}} = {#rec{field1=123}}.
