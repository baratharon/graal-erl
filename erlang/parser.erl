-module(parser).
-export([parse/1, ast/1, ast/2, ast/3, gen_ast/1, scan/1, scan_forms/1]).

scan(FileName) ->
	{ok, File} = file:read_file(FileName),
	{ok, Tokens, _} = erl_scan:string(binary_to_list(File)),
	Tokens.

scan_forms(FileName) ->
	{ok, File} = file:read_file(FileName),
	scan_forms(FileName, unicode:characters_to_list(File, utf8)).
	%scan_forms(FileName, binary_to_list(File)).

scan_forms(_, Data) ->
	scan_forms_impl(erl_scan:tokens([], Data, 1, []), []).

scan_forms_impl({done, {ok, T, N}, S}, Res) ->
	scan_forms_impl(erl_scan:tokens([], S, N, []), [T | Res]);
scan_forms_impl(_, Res) ->
	lists:reverse(Res).

gen_ast([]) ->
	ok;
gen_ast([ModuleName, AstFileName, SourceFileName | Tail]) ->
	ast(ModuleName, AstFileName, SourceFileName),
	gen_ast(Tail).

ast(ModuleName) when is_atom(ModuleName) ->
	ast(ModuleName, atom_to_list(ModuleName) ++ ".ast");
ast(ModuleName) ->
	ast(list_to_atom(ModuleName), ModuleName ++ ".ast").

ast(ModuleName, AstFileName) when is_atom(ModuleName) ->
	ast(ModuleName, AstFileName, atom_to_list(ModuleName) ++ ".erl");
ast(ModuleName, AstFileName) ->
	ast(list_to_atom(ModuleName), AstFileName, ModuleName ++ ".erl").

ast(ModuleName, AstFileName, SourceFileName) when is_atom(ModuleName) ->
	{AST, State} = parse(SourceFileName),
	Imports = maps:get(imports, State, []),
	file:write_file(AstFileName, io_lib:format("~w\n~w\n~p\n", [ModuleName, Imports, AST]));
ast(ModuleName, AstFileName, SourceFileName) ->
	ast(list_to_atom(ModuleName), AstFileName, SourceFileName).

parse(FileName) when is_atom(FileName) ->
	parse(atom_to_list(FileName) ++ ".erl");
parse(FileName) ->
	Forms = scan_forms(FileName),
	file:write_file("/tmp/forms", io_lib:format("~p~n", [Forms])),
	preprocess_and_parse([], Forms, #{}).

parse_and_extract_form(Form) ->
	% io:format("form: ~p~n", [Form]),
	case erl_parse:parse_form(Form) of
		{ok, ParsedForm} ->
			ParsedForm;
		{error, {Line, _, Problem}} ->
			io:format("Line: ~p~nProblem: ~p~nForm: ~p~n", [Line, Problem, Form]),
			erlang:throw(parse_error)
	end.

preprocess_and_parse(RevAcc, [[{'-',_} | RestOfForm] | Tail], State) ->
	% we found a '-' symbol, which is a directive, try to read it
	{ok, ReadAttributes, Remaining} = read_attribute(preprocess([], RestOfForm, State, soft), Tail),
	continue_preprocess_and_parse(RevAcc, Remaining, ReadAttributes, State);
preprocess_and_parse(RevAcc, [Head | Tail], State) ->
	% we found a form, preprocess, and parse it
	preprocess_and_parse([parse_and_extract_form(preprocess([], Head, State, hard)) | RevAcc], Tail, State);
preprocess_and_parse(RevAcc, [], State) ->
	{lists:reverse(RevAcc), State}.

preprocess(RevAcc, [{'?', _}, {var, Line, Name} | MoreTokens], State, Mode) ->
	{ok,  Resolved, RestTokens} = resolve(Name, Line, MoreTokens, State, Mode),
	case Mode of
		hard -> preprocess(RevAcc, lists:append(lists:reverse(Resolved), RestTokens), State, Mode);
		soft -> preprocess(lists:append(Resolved, RevAcc), RestTokens, State, Mode);
		_    -> {error, unknown_mode, Mode}
	end;
preprocess(RevAcc, [Token | MoreTokens], State, Mode) ->
	preprocess([Token | RevAcc], MoreTokens, State, Mode);
preprocess(RevAcc, [], _, _) ->
	lists:reverse(RevAcc).

continue_preprocess_and_parse(RevAcc, Forms, [{macro, MacroName, MacroDef} | Tail], State) ->
	% simple macro attribute found
	Macros = maps:get(macros, State, #{}),
	continue_preprocess_and_parse(RevAcc, Forms, Tail, State#{macros => Macros#{MacroName => MacroDef}});
continue_preprocess_and_parse(RevAcc, Forms, [{parametric, MacroName, MacroArgs, MacroDef} | Tail], State) ->
	% parametric macro attribute found
	Parametrics = maps:get(parametrics, State, #{}),
	continue_preprocess_and_parse(RevAcc, Forms, Tail, State#{parametrics => Parametrics#{MacroName => {MacroArgs, MacroDef}}});
continue_preprocess_and_parse(RevAcc, Forms, [{import, ImportModuleName, ImportFAs} | Tail], State) ->
	% import attribute found
	Imports = maps:get(imports, State, []),
	NewImports = register_imports(Imports, ImportModuleName, ImportFAs),
	continue_preprocess_and_parse(RevAcc, Forms, Tail, State#{imports => NewImports});
continue_preprocess_and_parse(RevAcc, Forms, [], State) ->
	% no unprocessed attribute left
	preprocess_and_parse(RevAcc, Forms, State).

register_imports(ImportList, ImportModuleName, [{FuncName, Arity} | FAs]) ->
	register_imports([{ImportModuleName, FuncName, Arity}|ImportList], ImportModuleName, FAs);
register_imports(ImportList, _ImportModuleName, []) ->
	ImportList.

resolve('LINE', Line, RestTokens, _State, _Mode) ->
	{ok, [{integer, Line, Line}], RestTokens};
resolve(NameToResolve, Line, RestTokens, State, Mode) ->
	{RMC, RMR} = resolve_macro(NameToResolve, RestTokens, State),
	case RMC of
		ok -> RMR;
		_  ->
			{RPC, RPR} = resolve_parametric(NameToResolve, RestTokens, State),
			case RPC of
				ok -> RPR;
				_  ->
					case Mode of
						soft -> {ok, [{var, Line, NameToResolve}, {'?', Line}], RestTokens};
						hard -> {error, {no_such_macro, NameToResolve, Line}}
					end
			end
	end.

resolve_macro(NameToResolve, RestTokens, State) ->
	case maps:find(macros, State) of
		{ok, Macros} ->
			case maps:find(NameToResolve, Macros) of
				{ok, Replacement} -> {ok, {ok, Replacement, RestTokens}};
				_ -> {error, RestTokens}
			end;
		error ->
			{error, RestTokens}
	end.

resolve_parametric(NameToResolve, RestTokens, State) ->
	case maps:find(parametrics, State) of
		{ok, Parametrics} ->
			case maps:find(NameToResolve, Parametrics) of
				{ok, {Args, Replacement}} -> collect_parametric_params(Args, Replacement, RestTokens);
				_ -> {error, RestTokens}
			end;
		error ->
			{error, RestTokens}
	end.

collect_parametric_params(Args, Replacement, [{'(', _} | RestTokens]) ->
	collect_parametric_params(#{}, [], Args, Replacement, RestTokens).

collect_parametric_params(ParamMap, [], [], Replacement, RestTokens) ->
	replace_parametric_macro(ParamMap, [], Replacement, RestTokens);
collect_parametric_params(ParamMap, Acc, [Arg|Args], Replacement, [{',', _} | RestTokens]) ->
	collect_parametric_params(ParamMap#{Arg => Acc}, [], Args, Replacement, RestTokens);
collect_parametric_params(ParamMap, Acc, [Arg|Args], Replacement, [{')', _} | RestTokens]) ->
	collect_parametric_params(ParamMap#{Arg => Acc}, [], Args, Replacement, RestTokens);
collect_parametric_params(ParamMap, Acc, Args, Replacement, [Token | RestTokens]) ->
	collect_parametric_params(ParamMap, [Token | Acc], Args, Replacement, RestTokens).

replace_parametric_macro(ParamMap, Acc, [Tok={var, _, Var} | RepTokens], RestTokens) ->
	case maps:find(Var, ParamMap) of
		{ok, ParamValue} -> replace_parametric_macro(ParamMap, ParamValue ++ Acc, RepTokens, RestTokens);
		_                -> replace_parametric_macro(ParamMap, [Tok | Acc], RepTokens, RestTokens)
	end;
replace_parametric_macro(ParamMap, Acc, [RepToken | RepTokens], RestTokens) ->
	replace_parametric_macro(ParamMap, [RepToken | Acc], RepTokens, RestTokens);
replace_parametric_macro(_ParamMap, Acc, [], RestTokens) ->
	{ok, {ok, Acc, RestTokens}}.

read_attribute([{atom, _, file}, {'(', _} | _], Forms) ->
	{ok, [], Forms};
read_attribute([{atom, _, module}, {'(', _}, {atom, _, ModuleName}, {')', _}, {dot, _} | []], Forms) ->
	% module name has a strict syntax (we do not match the module name and the file name)
	{ok, [{macro, 'MODULE', [{atom, -1, ModuleName}]}, {macro, 'MODULE_STRING', [{string, -1, atom_to_list(ModuleName)}]}], Forms};
read_attribute([{atom, Line, define}, {'(', _}, {var, _, DefName}, {',', _} | DefTail], Forms) ->
	% defining simple macros
	validate_def_tail(DefName, Line, lists:reverse(DefTail), Forms);
read_attribute([{atom, Line, define}, {'(', _}, {var, _, DefName}, {'(', _} | DefTail], Forms) ->
	% defining parametric macros
	parse_parametric_macro(DefName, Line, DefTail, Forms);
read_attribute([{atom, _, export} | _], Forms) ->
	% drop the 'export' now, we don't need it yet
	{ok, [], Forms};
read_attribute([{atom, _, export_type} | _], Forms) ->
	% drop the 'export_type' now, we don't need it yet
	{ok, [], Forms};
read_attribute([{atom, _, type} | _], Forms) ->
	% drop the 'type' now, we don't need it yet
	{ok, [], Forms};
read_attribute([{atom, _, opaque} | _], Forms) ->
	% drop the 'opaque' now, we don't need it yet
	{ok, [], Forms};
read_attribute([{atom, _, import} | Rest], Forms) ->
	% parse the import attribute
	parse_imports(Rest, Forms);
read_attribute([{atom, _, compile} | _], Forms) ->
	% drop the 'compile' now, we don't need it yet
	{ok, [], Forms};
read_attribute([{atom, _, spec} | _], Forms) ->
	% drop the 'spec' now, we don't need it yet
	{ok, [], Forms};
read_attribute([{atom, _, deprecated} | _], Forms) ->
	% drop the 'deprecated' now, we don't need it yet
	{ok, [], Forms};
read_attribute([{atom, Line, AtomName} | _Rest], _) ->
	% not supported feature
	%io:format("rest: ~p~n", [_Rest]),
	{error, {unknown_atom, Line, AtomName}};
read_attribute(_, _) ->
	% we expect at least an atom here
	{error, {expected_an_atom}}.

validate_def_tail(DefName, _, [{dot, _}, {')', _} | ReversedReplacement], Forms) ->
	{ok, [{macro, DefName, [{')',-1}] ++ ReversedReplacement ++ [{'(',-1}]}], Forms};
validate_def_tail(_, Line, _, _) ->
	{error, {illformed_define, Line}}.

parse_parametric_macro(DefName, Line, DefTail, Forms) ->
	parse_parametric_macro(DefName, [], Line, DefTail, Forms).

parse_parametric_macro(DefName, RevArgs, Line, [{var, _, ArgName}, {',', _} | DefTail], Forms) ->
	parse_parametric_macro(DefName, [ArgName | RevArgs], Line, DefTail, Forms);
parse_parametric_macro(DefName, RevArgs, _Line, [{var, _, ArgName}, {')', _}, {',', _} | DefTail], Forms) ->
	parse_parametric_macro_tail(DefName, lists:reverse([ArgName | RevArgs]), lists:reverse(DefTail), Forms).

parse_parametric_macro_tail(DefName, Args, [{dot, _}, {')', _} | ReversedReplacement], Forms) ->
	{ok, [{parametric, DefName, Args, [{'(',-1} | lists:reverse([{')',-1} | ReversedReplacement])]}], Forms};
parse_parametric_macro_tail(DefName, _, _, _) ->
	{error, {illformed_define, xxx_TODO_Line, DefName}}. % TODO

parse_imports([{'(', _}, {atom, _, ModuleName}, {',', _}, {'[', _} | Rest], Forms) ->
	parse_imports(ModuleName, Rest, [], Forms).

parse_imports(ModuleName, [{atom, _, Func}, {'/', _}, {integer, _, Arity} | Rest], FAs, Forms) ->
	parse_imports(ModuleName, Rest, [{Func, Arity} | FAs], Forms);
parse_imports(ModuleName, [{',', _} | Rest], FAs, Forms) ->
	parse_imports(ModuleName, Rest, FAs, Forms);
parse_imports(ModuleName, [{']', _}, {')', _}, {dot, _}], FAs, Forms) ->
	{ok, [{import, ModuleName, FAs}], Forms}.
