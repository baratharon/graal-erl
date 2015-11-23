-module(builtin54).
-export([main/0]).

main() ->
	St0 = erlang:md5_init(),
	erlang:md5_final(St0).
