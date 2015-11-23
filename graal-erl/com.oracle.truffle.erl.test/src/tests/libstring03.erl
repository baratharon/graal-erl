-module(libstring03).
-export([main/0]).
-define(STRING,string).
-import(?STRING,[chars/3]).

main() ->
	chars($A, 15, ".").
