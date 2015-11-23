-module(builtin55).
-export([main/0]).

main() ->
	St0 = erlang:md5_init(),
	St1 = erlang:md5_update(St0, <<1>>),
	St2 = erlang:md5_update(St1, [<<2,3>>,4]),
	St3 = erlang:md5_update(St2, [[<<5>>],<<6, 7>>]),
	{
		erlang:md5_final(St1),
		erlang:md5_final(St2),
		erlang:md5_final(St3)
	}.
