-module(case04).
-export([main/0]).

main() ->
	Ss = [12, {tag, 1}, 4],
	[case S of
		{tag, X} -> {tagged, X};
		Other    -> {other, Other}
	end || S <- Ss].
