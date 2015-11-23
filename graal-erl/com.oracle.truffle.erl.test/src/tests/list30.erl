-module(list30).
-export([main/0]).

base_list() ->
	[7, 19, 10, 16, 9, 13, 3, 12, 2, 6, 17, 20, 5, 8, 18, 11, 1, 14, 4, 15].

sort([Pivot|T]) ->
	sort([ X || X <- T, X < Pivot]) ++
	[Pivot] ++
	sort([ X || X <- T, X >= Pivot]);
sort([]) ->
	[].

main() ->
	sort(base_list()).
