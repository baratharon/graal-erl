-module(dict07).
-export([main/0]).

main() ->
	erase(),
	Common = {1, 2},
	put(key1, Common),
	put(key2, {1, 1}),
	put(key3, Common),
	put(key4, Common),
	get_keys(Common).
