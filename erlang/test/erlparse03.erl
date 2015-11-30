-module(erlparse03).
-export([main/0, main/1, parse/1]).

main([H|T]) ->
	parse(H),
	main(T);
main([]) ->
	ok.

main() ->
	parse("{1,2,[hello]," ?MODULE_STRING ":function()}.").

parse(Atom) when is_atom(Atom) ->
	parse(atom_to_list(Atom));
parse(String) ->
	{ok, Tokens, _EndLine} = erl_scan:string(String),
	{ok, AbsForm} = erl_parse:parse_exprs(Tokens),
	{value, Value, _Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
	io:format("parsed: ~p~n", [Value]).
