-module(compare04).
-export([main/0]).

main() ->
	[
		[1] == [1.0],
		[1] /= [1.0],
		[1] =:= [1.0],
		[1] =/= [1.0],
		[10|20] == [10|21],
		[10|21] == [10|21]
	].
