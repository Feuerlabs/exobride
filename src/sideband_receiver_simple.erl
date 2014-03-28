%%%-------------------------------------------------------------------
%%% @author magnus <magnus@t520.home>
%%% @copyright (C) 2014, magnus
%%% @doc
%%%
%%% @end
%%% Created : 11 Mar 2014 by magnus <magnus@t520.home>
%%%-------------------------------------------------------------------
-module(sideband_receiver_simple).

-behaviour(gen_server).
-behaviour(sideband_receiver_plugin).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% router_server_plugin callback
-export([start/3, 
	 stop/0]).

-define(SERVER, ?MODULE). 
-define(RECEIVER_PORT_DEFAULT, 5715).

-record(st, {
	  sock = nil,
	  cb_pid = nil,
	  cb_arg = nil
	 }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start(CBPid, CBArg, Opts) ->
    { ok, _ } = gen_server:start_link({local, ?SERVER}, ?MODULE, {CBPid, CBArg, Opts}, []),
    ok.

stop() ->
    gen_server:call(?SERVER, sideband_stop),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init({CBPid, CBArg, Opts}) ->
    Port = util:get_opt(receiver_port, Opts, ?RECEIVER_PORT_DEFAULT),
    case gen_udp:open(Port,  [list, { active, true }]) of
	{ ok, Socket } ->
	    {ok, #st{ cb_pid = CBPid, cb_arg = CBArg, sock = Socket }};
	
	{ error, Reason} ->
	    {stop, Reason }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(sideband_stop, _From, St) ->
    gen_udp:close(St#st.sock),
    { stop, ok, St };

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info( {udp, _Socket, IP, Port, Data }, St) ->
    io:format("sideband_receiver_simple: ~p:~p sent ~p as a wakeup~n", 
	      [ IP, Port, Data ]),
    St#st.cb_pid ! { sideband_wakeup, 
		     self(), St#st.cb_arg, 
		     IP, Port, Data }, 

    { noreply, St };

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


