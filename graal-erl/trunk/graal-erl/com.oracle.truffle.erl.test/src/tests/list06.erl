-module(list06).
-export([main/0, len/1]).

len([]) ->
	0;
len([_ | T]) ->
	len_impl(1, T);
len(_) ->
	error.

len_impl(Acc, []) ->
	Acc;
len_impl(Acc, [_ | T]) ->
	len_impl(Acc+1, T).

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
