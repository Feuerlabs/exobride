-module(bride_plugin).

-export_type([options/0, data/0, error/0, ok/0, ref/0]).

-type options()     :: [{atom(), any()}].
-type data()        :: any().
-type error()       :: {error, any()}.
-type ok()          :: {ok, data()}.
-type callback_pid()         :: pid().
-type ref()         :: pid().
-type callback_ref()         :: any().

-callback start(options()) ->
    { ok, ref() } | error().

-callback connect_local_server(ref(), 
			       callback_pid(),
			       callback_ref()) ->
     ok | error().

-callback send_local_server(ref(), data()) ->
    ok  | error().

-callback disconnect_local_server(ref()) ->
    ok.
