-module(router_client_plugin).

-export_type([options/0, data/0, 
	      error/0, ok/0, ref/0,
	     cb_pid/0, cb_arg/0]).

-type options()     :: [{atom(), any()}].
-type data()        :: any().
-type error()       :: {error, any()}.
-type ok()          :: {ok, data()}.
-type ref()         :: pid().
-type cb_pid()      :: pid().
-type cb_arg()      :: any().
-type key()         :: any().

-callback start(cb_pid(), cb_arg(), key(), options()) ->
    { ok, ref() } | error().

-callback send_server(ref(), data()) ->
    ok  | error().

-callback disconnect_server(ref()) ->
    ok.



