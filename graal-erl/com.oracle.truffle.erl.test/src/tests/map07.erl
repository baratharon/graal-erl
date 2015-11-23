-module(map07).
-export([main/0]).

main() ->
	Map = #{ old => 2000} #{ a => 10, 10 => a, 1.1 => fun(X) -> X*2 end } #{ new => 4000 },
	maps:keys(Map).
