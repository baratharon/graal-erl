-module(bin50).
-export([main/0]).

f(Bin) ->
	<<Bin/binary-unit:1>>.

main() ->
	{
		f(<<>>),
		f(<<1>>),
		f(<<1, 2>>),
		f(<<1, 2, 3>>)
	}.
