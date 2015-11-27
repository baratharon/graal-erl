-module(record23).
-export([main/0]).
-record(state, {bla1, loader, bla2}).

-define(SAFE2(Expr, State), 
        fun() ->
                case catch Expr of
                    {'EXIT',XXXReason} -> {{error,XXXReason}, State};
                    XXXRes -> XXXRes
                end
        end()).

efile_get_file_from_port(State, File, Paths) ->
	{State, File, Paths}.

inet_get_file_from_port(State, File, Paths) ->
	{State, File, Paths}.

main() ->
    handle_get_file(#state{bla1=[], loader=efile, bla2={}}, ".", "tmp").

handle_get_file(State = #state{loader = efile}, Paths, File) ->
    ?SAFE2(efile_get_file_from_port(State, File, Paths), State);
handle_get_file(State = #state{loader = inet}, Paths, File) ->
    ?SAFE2(inet_get_file_from_port(State, File, Paths), State).
