-module(spawn07).
-export([main/0, child/1]).

child(X) ->
	receive
		{Sender, atom} -> Sender ! {self(), X}
	end.

main() ->
	Secret = secret,
	Pid = spawn(fun() -> child(Secret) end),
	Pid ! {self(), atom},
	receive
		{Pid2, Secret} -> true = Pid =:= Pid2, {yay, Secret}
	end.
