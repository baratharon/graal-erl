-module(match13).
-export([main/0]).

f(#{b:=B} = R) ->
	{B, R}.

main() ->
	f(#{a=>1,b=>2,c=>3}).
