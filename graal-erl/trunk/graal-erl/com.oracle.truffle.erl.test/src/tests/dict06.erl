-module(dict06).
-export([main/0]).

main() ->
	erase(),
	put(key1, world1),
	put(key2, world2),
	put(key3, world3),
	put(key2, world4),
	get_keys().
