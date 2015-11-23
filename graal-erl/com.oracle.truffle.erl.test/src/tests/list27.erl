-module(list27).
-export([main/0]).

main() ->
	[X || true, begin X=1, true end].
