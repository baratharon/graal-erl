-module(list33).
-export([main/0]).

my_seq(From, To) when From=<To ->
	my_seq(From, To, []);
my_seq(_, _) ->
	[].

my_seq(From, From, Acc) ->
	[From | Acc];
my_seq(From, To, Acc) ->
	my_seq(From, To-1, [To | Acc]).

main() ->
	{my_seq(1, 5), my_seq(3, 5), my_seq(4, 4), my_seq(4, 3)}.
