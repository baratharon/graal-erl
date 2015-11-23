-module(if02).
-export([main/0]).

f(First, Last, Inc) ->
	if
		Inc > 0, First - Inc =< Last;
		Inc < 0, First - Inc >= Last ->
			N = (Last - First + Inc) div Inc,
			{ok1, N};
		Inc =:= 0, First =:= Last ->
			ok2
	end.

main() ->
	f(10, 20, 2).
