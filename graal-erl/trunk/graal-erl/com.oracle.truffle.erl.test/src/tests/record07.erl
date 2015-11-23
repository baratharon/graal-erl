-module(record07).
-export([main/0]).
-record(rec, {field1, field2}).

main() ->
	{#rec{field1=123}} = {#rec{field2=atom, field1=123}}.
