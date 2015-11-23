-module(spawn01).
-export([main/0, child/0]).

child() ->
	ok.

wait_for_terminate(Pid) ->
	case
		is_process_alive(Pid)
	of
		true ->
			erlang:yield(),
			wait_for_terminate(Pid);
		false ->
			ok
	end.

main() ->
	Pid = spawn(fun child/0),
	true = is_pid(Pid),
	wait_for_terminate(Pid),
	{ok}.
