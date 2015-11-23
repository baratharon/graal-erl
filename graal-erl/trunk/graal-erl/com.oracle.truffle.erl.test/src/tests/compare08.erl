-module(compare08).
-export([main/0]).

main() ->
	{
		by_size,
		#{} < #{a=>1},
		#{} == #{a=>1},
		by_keys,
		#{a=>1} < #{b=>2},
		#{a=>1} == #{b=>2},
		by_values,
		#{a=>1} < #{a=>2},
		#{a=>1} == #{a=>2}
	}.
