-module(libmaps01).
-export([main/0]).

main() ->
	Map = #{42 => value_three,666 => {value,two},a => 1},
	Ks = [a,42,{other,key}],
	maps:with(Ks,Map).
