-module(erlparse02).
-export([main/0, function/0]).

function() ->
	function_called.

main() ->
	{ok, Tokens, _EndLine} = erl_scan:string("{1,2,[hello]," ?MODULE_STRING ":function()}."),
	{ok, AbsForm} = erl_parse:parse_exprs(Tokens),
	{value,Value, _Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
	io:format("value: ~p~n", [Value]).
