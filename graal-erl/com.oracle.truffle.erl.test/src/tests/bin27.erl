-module(bin27).
-export([main/0]).

triples_to_bin(T) ->
	triples_to_bin(T, <<>>).

triples_to_bin([{X,Y,Z} | T], Acc) ->
	triples_to_bin(T, <<Acc/binary,X:32,Y:32,Z:32>>);   % inefficient before R12B
triples_to_bin([], Acc) -> 
	Acc.

main() ->
	triples_to_bin([{1,2,3},{100,200,300}]).
