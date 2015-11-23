-module(bin36).
-export([main/0]).

main() ->
	<< << (X*2) >> || <<X>> <= << 1,2,3 >> >>.
