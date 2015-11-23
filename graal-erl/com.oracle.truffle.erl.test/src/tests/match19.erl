-module(match19).
-export([main/0]).

f(<<"-run">>) ->
	run;
f(_) ->
	other.

main() ->
	Bin1 = <<"-RUN">>,
	Bin2 = <<"-run">>,
	{
		f(Bin1),
		f(Bin2),
		f(atom)
	}.
