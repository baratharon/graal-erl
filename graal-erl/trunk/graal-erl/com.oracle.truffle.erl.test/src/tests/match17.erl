-module(match17).
-export([main/0]).

f(L = (#{b:=B} = R)) ->
	{L, B, R}.

main() ->
	f(#{a=>1,b=>2}).
