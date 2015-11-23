-module(list32).
-export([main/0]).

perms([]) ->
	[[]];
perms(L) ->
	[[H|T] || H <- L, T <- perms(L--[H])].

main() ->
	perms([b, u, g]).
