-module(sideband_receiver_plugin).

-export_type([options/0, cb_arg/0, error/0]).

-type options()     :: [{atom(), any()}].
-type cb_arg()      :: any().
-type error()       :: {error, any()}.

-callback start(pid(), cb_arg(), options()) ->
    ok | error().

-callback stop() ->
    ok | error().
