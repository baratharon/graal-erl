-module(record18).
-export([main/0]).

-record(state, {connect_all        :: boolean(),
		known = []         :: [node()],
		synced = []        :: [node()],
		resolvers = [],
		syncers = []       :: [pid()],
		node_name = node() :: node(),
		the_locker, the_registrar, trace,
                global_lock_down = false :: boolean()
               }).

start_the_locker(DoTrace) ->
	DoTrace.

start_the_registrar() ->
	ok.

send_high_level_trace() ->
	trace.

match(#state{trace=trace}) -> [trace];
match(#state{trace=no_trace}) -> [no_trace].

main() ->
    DoTrace = false,

    T0 = case DoTrace of
             true ->
                 send_high_level_trace(),
                 [];
             false ->
                 no_trace
         end,

    S = #state{the_locker = start_the_locker(DoTrace),
               trace = T0,
               the_registrar = start_the_registrar()},
    match(S).
