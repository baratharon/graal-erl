-module(onload).
-export([main/0, main/1]).
-on_load(onload/0).
-define(ON_LOAD_KEY, my_key).

onload() ->
	put(?ON_LOAD_KEY, yup),
	ok.

main(_) ->
	main().

main() ->
	io:format("~p~n", [get(?ON_LOAD_KEY)]).
