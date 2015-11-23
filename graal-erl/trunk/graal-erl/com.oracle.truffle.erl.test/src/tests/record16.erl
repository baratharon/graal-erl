-module(record16).
-export([main/0]).
-record(rec, {}).

main() ->
	#rec{}.
