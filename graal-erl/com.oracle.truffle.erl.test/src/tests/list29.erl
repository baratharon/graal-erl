-module(list29).
-export([main/0]).

base_list() ->
	[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14].

gcd(A, A) ->
	A;
gcd(A, B) when A > B ->
	gcd(A-B, B);
gcd(A, B) ->
	gcd(A, B-A).

main() ->
	[GCD || X <- base_list(), Y <- base_list(), X > Y,
		begin GCD = gcd(X, Y), 1 /= GCD end].
