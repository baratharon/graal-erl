-module(bin48).
-export([main/0]).

f(<<>>) ->
	zero;
f(<<_>>) ->
	one;
f(<<_, _>>) ->
	two;
f(<<_, _, _>>) ->
	three.

main() ->
	{
		f(<<1,2,3>>),
		f(<<1>>),
		f(<<>>)
	}.
