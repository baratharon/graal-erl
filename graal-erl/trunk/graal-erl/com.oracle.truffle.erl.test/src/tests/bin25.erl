-module(bin25).
-export([show/1, main/0]).

show(List) when is_list(List) ->
	[show(Bin) || Bin <- List];
show([]) ->
	[];
show(<<Bin/bitstring>>) ->
	show(Bin, []);
show(<<>>) ->
	[].

show(<<Bit:1, Rest/bitstring>>, Acc) ->
	%io:format("Rest: ~p~n", [Rest]),
	show(Rest, [Bit | Acc]);
show(<<>>, Acc) ->
	lists:reverse(Acc, []).

main() ->
	{
		show(<<11,22,1:3>>),
		show(<< (-13) : 17>>)
	}.
