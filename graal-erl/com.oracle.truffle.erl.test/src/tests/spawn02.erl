-module(spawn02).
-export([main/0, child/0]).

child() ->
	receive
		{Sender, atom} -> Sender ! self()
	end.

main() ->
	Pid = spawn(fun child/0),
	Pid ! {self(), atom},
	receive
		Pid2 -> true = Pid =:= Pid2, yay
	end.
