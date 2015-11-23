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
preprocess(RevAcc, [{'#', _}, {atom, Line, RecName}, {'{', _} | MoreTokens], State, Mode) ->
	case head_is_end_of_expr(RevAcc) of
		true ->
			{Expr, NewRevAcc} = fetch_last_expr(RevAcc),
			{ok, Replacement, RestTokens} = resolve_record_update(RecName, Expr, Line, MoreTokens, State);
		false ->
			NewRevAcc = RevAcc,
			{ok, Replacement, RestTokens} = resolve_record_expr(RecName, Line, MoreTokens, State)
	end,
	preprocess(lists:append(Replacement, NewRevAcc), RestTokens, State, Mode);
%% TODO: ha ')', '}' vagy var van elotte, akkor update (mar preprocesszalt, de ez mindegy is)
%% TODO: ugyanez csak '=' helyett '.' -> member access
preprocess(RevAcc, [Token | MoreTokens], State, Mode) ->
	preprocess([Token | RevAcc], MoreTokens, State, Mode);
preprocess(RevAcc, [], _State, _Mode) ->
	lists:reverse(RevAcc).

continue_preprocess_and_parse(RevAcc, Forms, [{macro, MacroName, MacroDef} | Tail], State) ->
	% simple macro attribute found
	Macros = maps:get(macros, State, #{}),
	continue_preprocess_and_parse(RevAcc, Forms, Tail, State#{macros => Macros#{MacroName => MacroDef}});
continue_preprocess_and_parse(RevAcc, Forms, [{parametric, MacroName, MacroArgs, MacroDef} | Tail], State) ->
	% parametric macro attribute found
	Parametrics = maps:get(parametrics, State, #{}),
	Overloads = maps:get(MacroName, Parametrics, #{}),
	Arity = length(MacroArgs),
	continue_preprocess_and_parse(RevAcc, Forms, Tail, State#{parametrics => Parametrics#{MacroName => Overloads#{Arity => {MacroArgs, MacroDef}}}});
continue_preprocess_and_parse(RevAcc, Forms, [{import, ImportModuleName, ImportFAs} | Tail], State) ->
	% import attribute found
	Imports = maps:get(imports, State, []),
	NewImports = register_imports(Imports, ImportModuleName, ImportFAs),
	continue_preprocess_and_parse(RevAcc, Forms, Tail, State#{imports => NewImports});
continue_preprocess_and_parse(RevAcc, Forms, [{record, RecordName, RecordInfo} | Tail], State) ->
	% record attribute found
	Records = maps:get(records, State, #{}),
	continue_preprocess_and_parse(RevAcc, Forms, Tail, State#{records => Records#{RecordName => RecordInfo}});
continue_preprocess_and_parse(RevAcc, Forms, [], State) ->
	% no unprocessed attribute left
	preprocess_and_parse(RevAcc, Forms, State).

register_imports(ImportList, ImportModuleName, [{FuncName, Arity} | FAs]) ->
	register_imports([{ImportModuleName, FuncName, Arity}|ImportList], ImportModuleName, FAs);
register_imports(ImportList, _ImportModuleName, []) ->
	ImportList.

head_is_end_of_expr([Head | _Tail]) ->
	case Head of
		{')', _}    -> true;
		{'}', _}    -> true;
		{']', _}    -> true;
		{var, _, _} -> true;
		_           -> false
	end;
head_is_end_of_expr(_) ->
	false.

fetch_last_expr(RevExprs) ->
	{Expr, RevTail} = fetch_last_expr([], RevExprs),
	{lists:reverse(Expr), RevTail}.

fetch_last_expr(Acc, [Head={Close, _} | Tail]) ->
	case Close of
		')' -> fetch_last_expr_impl([Head | Acc], Tail, 0, '(', ')', fun fetch_paren_head/2);
		'}' -> fetch_last_expr_impl([Head | Acc], Tail, 0, '{', '}', fun fetch_brace_head/2);
		']' -> fetch_last_expr_impl([Head | Acc], Tail, 0, '[', ']', undefined)
	end;
fetch_last_expr(Acc, [Head={var, _, _} | Tail]) ->
	{[Head | Acc], Tail}.

fetch_paren_head(Acc, [H1={atom, _, _}, H2={':', _}, H3={atom, _, _} | Tail]) ->
	% remote function call
	{[H3, H2, H1 | Acc], Tail};
fetch_paren_head(Acc, [H1={Type, _, _} | Tail]) when atom==Type; var==Type ->
	% local function call, or fun variable
	{[H1 | Acc], Tail};
fetch_paren_head(Acc, Tail) ->
	% simple parenthesis
	{Acc, Tail}.

fetch_brace_head(Acc, [H1={'#', _} | Tail]) ->
	case head_is_end_of_expr(Tail) of
		true -> fetch_last_expr([H1 | Acc], Tail);
		_    -> {[H1 | Acc], Tail}
	end;
fetch_brace_head(Acc, Tail) ->
	% tuple
	{Acc, Tail}.

fetch_last_expr_impl(Acc, [Head={Open, _} | Tail], 0, Open, _Close, Fun) ->
	case is_function(Fun) of
		true -> Fun([Head | Acc], Tail);
		_    -> {[Head | Acc], Tail}
	end;
fetch_last_expr_impl(Acc, [Head={Open, _} | Tail], Depth, Open, Close, Fun) ->
	fetch_last_expr_impl([Head | Acc], Tail, Depth+1, Open, Close, Fun);
fetch_last_expr_impl(Acc, [Head={Close, _} | Tail], Depth, Open, Close, Fun) ->
	fetch_last_expr_impl([Head | Acc], Tail, Depth-1, Open, Close, Fun);
fetch_last_expr_impl(Acc, [Head | Tail], Depth, Open, Close, Fun) ->
	fetch_last_expr_impl([Head | Acc], Tail, Depth, Open, Close, Fun).

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
				{ok, Overloads} -> collect_parametric_params(Overloads, RestTokens);
				_ -> {error, RestTokens}
			end;
		error ->
			{error, RestTokens}
	end.

collect_parametric_params(Overloads, [{'(', _} | RestTokens]) ->
	collect_parametric_params(Overloads, [], [], 0, RestTokens).

collect_parametric_params(Overloads, Actuals, Acc, 0=_Level, [{',', _} | RestTokens]) ->
	collect_parametric_params(Overloads, [Acc | Actuals], [], 0, RestTokens);
collect_parametric_params(Overloads, Actuals, Acc, 0=_Level, [{')', _} | RestTokens]) ->
	NewActuals = build_actual_macro_params(Acc, Actuals),
	ActualArity = length(NewActuals),
	case maps:find(ActualArity, Overloads) of
		{ok, {MacroParamNames, MacroReplacement}} ->
			ParamMap = build_param_map(MacroParamNames, NewActuals),
			replace_parametric_macro(ParamMap, [], MacroReplacement, RestTokens);
		_ -> {error, no_overload_found, ActualArity}
	end;
collect_parametric_params(Overloads, Actuals, Acc, Level, [Token={',', _} | RestTokens]) ->
	collect_parametric_params(Overloads, Actuals, [Token | Acc], Level, RestTokens);
collect_parametric_params(Overloads, Actuals, Acc, Level, [Token={')', _} | RestTokens]) ->
	collect_parametric_params(Overloads, Actuals, [Token | Acc], Level-1, RestTokens);
collect_parametric_params(Overloads, Actuals, Acc, Level, [Token={'(', _} | RestTokens]) ->
	collect_parametric_params(Overloads, Actuals, [Token | Acc], Level+1, RestTokens);
collect_parametric_params(Overloads, Actuals, Acc, Level, [Token | RestTokens]) ->
	collect_parametric_params(Overloads, Actuals, [Token | Acc], Level, RestTokens).

replace_parametric_macro(ParamMap, Acc, [Tok={var, _, Var} | RepTokens], RestTokens) ->
	case maps:find(Var, ParamMap) of
		{ok, ParamValue} -> replace_parametric_macro(ParamMap, ParamValue ++ Acc, RepTokens, RestTokens);
		_                -> replace_parametric_macro(ParamMap, [Tok | Acc], RepTokens, RestTokens)
	end;
replace_parametric_macro(ParamMap, Acc, [RepToken | RepTokens], RestTokens) ->
	replace_parametric_macro(ParamMap, [RepToken | Acc], RepTokens, RestTokens);
replace_parametric_macro(_ParamMap, Acc, [], RestTokens) ->
	%io:format("RPM: ~p, ~p~n", [Acc, RestTokens]),
	{ok, {ok, Acc, RestTokens}}.

build_param_map(Names, Values) ->
	build_param_map(#{}, Names, Values).

build_param_map(Map, [Name|Names], [Value|Values]) ->
	build_param_map(Map#{Name => Value}, Names, Values);
build_param_map(Map, [], []) ->
	Map.

build_actual_macro_params([], []) ->
	[];
build_actual_macro_params([], Actuals) ->
	lists:reverse(Actuals);
build_actual_macro_params(Acc, Actuals) ->
	lists:reverse([Acc | Actuals]).

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
read_attribute([{atom, _, record}, {'(', _}, {atom, _, RecName}, {',', _}, {'{', _} | Tail], Forms) ->
	% defining record (will be translated)
	parse_record(RecName, Tail, Forms);
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
	io:format("rest: ~p~n", [_Rest]),
	{error, {unknown_attribute, Line, AtomName}};
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

parse_record(RecName, Tail, Forms) ->
	parse_record(RecName, #{}, [], 2, Tail, Forms).

parse_record(RecName, FieldMap, FieldList, NextFieldId, [{atom, _, Field}, {',', _} | Tail], Forms) ->
	parse_record(RecName, FieldMap#{Field => {NextFieldId, undefined}}, [Field | FieldList], NextFieldId+1, Tail, Forms);
parse_record(RecName, FieldMap, FieldList, NextFieldId, [{atom, _, Field}, {'}', _} | Tail], Forms) ->
	build_record_info(RecName, FieldMap#{Field => {NextFieldId, undefined}}, [Field | FieldList], NextFieldId, Tail, Forms).

build_record_info(RecName, FieldMap, FieldList, Size, [{')', _}, {dot, _}], Forms) ->
	{ok, [{record, RecName, {RecName, Size, FieldMap, FieldList}}], Forms}.

resolve_record_update(RecName, Expr, Line, MoreTokens, State) ->
	{ok, Records} = maps:find(records, State),
	{ok, {RecName, _Size, FieldMap, _FieldList}} = maps:find(RecName, Records),
	resolve_record_update_impl(Expr, Line, [], FieldMap, MoreTokens).

resolve_record_update_impl(Expr, _Line, _ModifiedFields, _FieldMap, [{'}', _} | RestTokens]) ->
	{ok, Expr, RestTokens};
resolve_record_update_impl(Expr, Line, ModifiedFields, FieldMap, [{atom, _, Field}, {'=', _} | MoreTokens]) ->
	{ValueExpr, MoreTokens2} = parse_record_field_assign(MoreTokens),
	{ok, {FieldId, _Initial}} = maps:find(Field, FieldMap),
	SetElementExpr = make_setelement_expr(FieldId, Expr, ValueExpr, Line),
	resolve_record_update_impl(SetElementExpr, Line, [Field | ModifiedFields], FieldMap, MoreTokens2).

resolve_record_expr(RecName, Line, MoreTokens, State) ->
	Records = maps:get(records, State, #{}),
	{ok, RecordInfo={RecName, _Size, _FieldMap, _FieldList}} = maps:find(RecName, Records),
	resolve_record_expr(RecName, RecordInfo, Line, #{}, MoreTokens).

resolve_record_expr(RecName, RecordInfo, Line, FieldAssigns, [{'}', _} | RestTokens]) ->
	{RecName, _Size, FieldMap, FieldList} = RecordInfo,
	FinalAssigns = maps:merge(FieldMap, FieldAssigns),
	{ok, make_tuple_from_record(RecName, FieldList, FinalAssigns, Line), RestTokens};
resolve_record_expr(RecName, RecordInfo, Line, FieldAssigns, [{atom, _, Field}, {'=', _} | MoreTokens]) ->
	{Expr, MoreTokens2} = parse_record_field_assign(MoreTokens),
	resolve_record_expr(RecName, RecordInfo, Line, FieldAssigns#{Field => Expr}, MoreTokens2);
resolve_record_expr(RecName, RecordInfo, Line, FieldAssigns, [{var, _, '_'}, {'=', _} | MoreTokens]) ->
	{Expr, MoreTokens2} = parse_record_field_assign(MoreTokens),
	{RecName, _Size, _FieldMap, FieldList} = RecordInfo,
	NewFieldAssigns = fill_remaining_field_assigns(FieldAssigns, FieldList, Expr),
	resolve_record_expr(RecName, RecordInfo, Line, NewFieldAssigns, MoreTokens2).

parse_record_field_assign(Tokens) ->
	parse_record_field_assign([], Tokens).

parse_record_field_assign(Acc, [{',', _} | RestTokens]) ->
	{Acc, RestTokens};
parse_record_field_assign(Acc, [{'}', _} | _] = RestTokens) ->
	{Acc, RestTokens};
parse_record_field_assign(Acc, [Token | Tokens]) ->
	parse_record_field_assign([Token | Acc], Tokens).

fill_remaining_field_assigns(Assigns, [], _Expr) ->
	Assigns;
fill_remaining_field_assigns(Assigns, [Field | Fields], Expr) ->
	case maps:find(Field, Assigns) of
		error -> fill_remaining_field_assigns(Assigns#{Field => Expr}, Fields, Expr);
		_     -> fill_remaining_field_assigns(Assigns, Fields, Expr)
	end.

make_tuple_from_record(RecordName, RevFieldList, Assigns, Line) ->
	make_tuple_from_record_impl([{atom, Line, RecordName}, {'{', Line}], lists:reverse(RevFieldList), Assigns, Line).

make_tuple_from_record_impl(Replacement, [Field | Fields], Assigns, Line) ->
	{ok, Value} = maps:find(Field, Assigns),
	case Value of
		{_FieldId, undefined} -> ActualValue = [{atom, Line, undefined}];
		_                     -> ActualValue = Value
	end,
	make_tuple_from_record_impl(ActualValue ++ [{',', Line}] ++ Replacement, Fields, Assigns, Line);
make_tuple_from_record_impl(Replacement, [], _Assigns, Line) ->
	[{'}', Line} | Replacement].

make_setelement_expr(FieldId, Expr, ValueExpr, Line) ->
	[
		{')', Line},
		{')', Line}
	]
	++ ValueExpr ++
	[
		{'(', Line},
		{',', Line},
		{')', Line}
	]
	++ Expr ++
	[
		{'(', Line},
		{',', Line},
		{integer, Line, FieldId},
		{'(', Line},
		{atom, Line, setelement},
		{':', Line},
		{atom, Line, erlang}
	].
