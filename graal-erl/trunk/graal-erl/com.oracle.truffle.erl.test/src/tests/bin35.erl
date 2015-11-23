-module(bin35).
-export([main/0]).

main() ->
	{
		size(<<>>),
		size(<<11>>),
		size(<<11, 22>>),
		size(<<11, 22:15>>),
		size(<<11, 22:16>>),
		size(<<11, 22:17>>),
		size(<<11, 22:18>>)
	}.
