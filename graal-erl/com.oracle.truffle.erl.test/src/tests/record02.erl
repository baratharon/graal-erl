-module(record02).
-export([main/0]).
-record(rec, {field1, field2}).

main() ->
	#rec{field2=atom, _=122+1}.
