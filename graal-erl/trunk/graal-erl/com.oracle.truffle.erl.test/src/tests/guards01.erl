-module(guards01).
-export([main/0]).

f(X, Y) when X<0, Y<0 ->
	{both_negative, {X, Y}};
f(X, Y) when X<0; Y<0 ->
	{negative, {X, Y}};
f(X, Y) when X>0, Y>0 ->
	{both_positive, {X, Y}};
f(X, Y) when X>0; Y>0 ->
	{positive, {X, Y}};
f(X, Y) ->
	{zero, {X, Y}}.

main() ->
	[f(3, 2), f(0, 1), f(-1, -10), f(-1, 1), f(0, 0), f(-1, 0)].
