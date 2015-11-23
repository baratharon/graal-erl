-module(bin44).
-export([main/0]).

main() ->
	<<Num1/utf16-little>> = <<1071124703:32>>,
	<<Num2/utf16-big>>    = <<3628064528:32>>,
	<<Num3/utf8>>         = <<4055088533:32>>,
	{
		Num1,
		Num2,
		Num3
	}.
