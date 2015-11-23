-module(bin37).
-export([main/0]).

main() ->
	Bin = <<1:7, 2:7, 3:7>>,
	<< << (X*2) >> || <<X:7>> <= Bin >>.
