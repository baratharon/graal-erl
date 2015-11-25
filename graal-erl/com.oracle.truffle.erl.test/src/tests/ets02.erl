-module(ets02).
-export([main/0]).

main() ->
	Table = ets:new(tab, [bag]),
	ets:insert(Table, {1, a}),
	ets:insert(Table, {1, a}),
	ets:insert(Table, {1, aa}),
	ets:insert(Table, {1, aa}),
	ets:insert(Table, {1.0, b}),
	ets:insert(Table, {1.0, bb}),
	{
		ets:lookup(Table, 1),
		ets:lookup(Table, 1.0)
	}.
