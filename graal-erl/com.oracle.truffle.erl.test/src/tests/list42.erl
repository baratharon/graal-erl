-module(list42).
-export([main/0]).

main() ->
	List = [ {tag1, false}, {tag2, true}, {tag3, false}, {tag4, true} ],
	{
		[ Tag || {Tag, true} <- List ],
		?MODULE:module_info(module)
	}.
