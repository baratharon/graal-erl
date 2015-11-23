-module(list07).
-export([rev/1, main/0]).

rev([]) ->
	[];
rev(List) ->
	rev_acc([], List).

rev_acc(Acc, []) ->
	Acc;
rev_acc(Acc, [Head | Tail]) ->
	rev_acc([Head | Acc], Tail).

main() ->
	{rev([1,4,3000,2,5]), rev([w,z,y,x]), rev([d,o,o,g,' ',s,i,' ',s,i,h,t])}.
