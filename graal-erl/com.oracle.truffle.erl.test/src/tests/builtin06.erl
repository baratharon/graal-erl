-module(builtin06).
-export([main/0]).

main() ->
	List = [a, b, c, d],
	[hd(List) | tl(List)].
