-module(bin42).
-export([main/0]).

main() ->
	{
		<<16#1ff10/utf16-little>>,
		<<16#1ff10/utf16-big>>,
		<<16#73f55/utf8>>
	}.
