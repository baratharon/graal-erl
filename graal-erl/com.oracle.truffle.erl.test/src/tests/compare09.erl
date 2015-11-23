-module(compare09).
-export([main/0]).

main() ->
	Pid = self(),
	{
		Pid == Pid,
		Pid /= Pid,
		Pid =:= Pid,
		Pid =/= Pid,
		Pid < atom,
		Pid < 9,
		Pid < [],
		Pid < {},
		Pid < #{}
	}.
