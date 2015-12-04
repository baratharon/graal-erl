-module(bin53).
-export([main/0]).

main() ->
	L1 = 10,
	L2 = 7,
	L3 = 3,
	{
		<< <<1,2,3,7:6>>:L1/binary-unit:2 >>,
		<< <<1,2,3,7:6>>:L2/binary-unit:3 >>,
		<< <<1,2,3,7:4>>:L3/binary-unit:7 >>
	}.
