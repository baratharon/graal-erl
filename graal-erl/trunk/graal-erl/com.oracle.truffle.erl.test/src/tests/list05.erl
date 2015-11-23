-module(list05).
-export([main/0, len/1]).

len([]) ->
	0;
len([_H | T]) ->
	1 + len(T);
len(_) ->
	error.

main() ->
	{
		len([]),
		len([1]),
		len([[]]),
		len([[], []]),
		len([[], {}, 1]),
		len([{}, {}, {}, {1,{2},3}]),
		'-- end --'
	}.
