-module(record19).
-export([main/0]).
-record(r1, {r1f1={r1f1}, r1f2={r1f2}}).
-record(r2, {r2f1={r2f1}, r2f2={r2f2}}).

main() ->
	#r1{r1f2 = #r2{r2f1 = my_field}}.
