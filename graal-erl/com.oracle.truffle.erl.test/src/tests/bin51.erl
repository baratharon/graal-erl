-module(bin51).
-export([main/0]).

main() ->
	{
		try << <<1,2>>/binary-unit:7 >> catch error:badarg -> {ok} end,
		<< <<1,2,3,7:6>>/binary-unit:2 >>,
		<< <<1,2,3,7:6>>/binary-unit:3 >>,
		<< <<1,2,3,7:4>>/binary-unit:7 >>
	}.
