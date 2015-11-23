-module(record13).
-export([main/0]).
-record(rec, {temp}).

main() ->
	Rec = the_rec(),
	N = 2,
	W1 = Rec#rec{temp = [x | lists:delete(N, Rec#rec.temp)]},
	W1.

the_rec() ->
	#rec{temp=[1,2,3]}.
