-module(compare07).
-export([main/0]).

main() ->
	{
		#{a=>b, a=>c, a=>3} <  #{a=>3.0},
		#{a=>b, a=>c, a=>3} =< #{a=>3.0},
		#{a=>b, a=>c, a=>3} >  #{a=>3.0},
		#{a=>b, a=>c, a=>3} >= #{a=>3.0}
	}.
