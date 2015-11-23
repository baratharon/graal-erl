-module(match14).
-export([main/0]).

f(#{b:=B,a:=A} = R) ->
	{A, B, R}.

main() ->
	f(#{a=>1,b=>2}).
