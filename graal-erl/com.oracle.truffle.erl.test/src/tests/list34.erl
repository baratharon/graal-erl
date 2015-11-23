-module(list34).
-export([main/0]).

my_seq(From, To) when From=<To ->
	my_seq(From, To, []);
my_seq(_, _) ->
	[].

my_seq(From, From, Acc) ->
	[From | Acc];
my_seq(From, To, Acc) ->
	my_seq(From, To-1, [To | Acc]).

pythagorean_triplets(N) ->
	[{A,B,C} ||
		A <- my_seq(1,N-2),
		B <- my_seq(A+1,N-1),
		C <- my_seq(B+1,N),
		A+B+C =< N,
		A*A+B*B == C*C
	].

main() ->
	pythagorean_triplets(50).
