-module(bin39).
-export([main/0]).

main() ->
	List = [11, 22, 33, 44, 55],
	<< << (1+X*2):7 >> || X <- List >>.
