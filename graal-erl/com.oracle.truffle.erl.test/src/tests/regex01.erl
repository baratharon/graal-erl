-module(regex01).
-export([main/0]).

main() ->
	re:run(<<1,2,3,4>>, <<3>>).
