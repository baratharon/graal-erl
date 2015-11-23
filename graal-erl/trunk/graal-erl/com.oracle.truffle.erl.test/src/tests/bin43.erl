-module(bin43).
-export([main/0]).

main() ->
	<<Num1:32>> = <<16#1ff10/utf16-little>>,
	<<Num2:32>> = <<16#1ff10/utf16-big>>,
	<<Num3:32>> = <<16#73f55/utf8>>,
	{
		Num1,
		Num2,
		Num3
	}.
