-module(record20).
-export([main/0]).
-record(r1, {r1f1={r1f1}, r1f2={r1f2}}).
-record(r2, {r2f1={r2f1}, r2f2={r2f2}}).

test(#r1{r1f2 = #r2{r2f1 = my_field}}) ->
	yup;
test(_) ->
	nope.

main() ->
	Rec = #r1{r1f2 = #r2{r2f1 = my_field}},
	test(Rec).
