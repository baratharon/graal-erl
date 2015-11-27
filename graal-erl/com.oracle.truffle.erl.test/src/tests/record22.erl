-module(record22).
-export([main/0, external/2]).
-record(state, {bla1, handle, bla2, unic}).

external(_Handle, _Chars) ->
	ok.

test(Chars, latin1, #state{handle=Handle, unic=latin1}=State) ->
	case ?MODULE:external(Handle, Chars) of
		{error, _}=Reply ->
		    {stop, normal, Reply, State};
		Reply ->
		    {reply, Reply, State}
	end;
test(_Chars, _InEncoding, #state{handle=_Handle, unic=_OutEncoding}=State) ->
	{reply, ok, State}.

main() ->
	test(<<100, 101, 102>>, unicode, #state{bla1=[], handle=[my_handle], bla2={}, unic=unicode}).
