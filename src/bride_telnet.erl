%%%-------------------------------------------------------------------
%%% @author magnus <magnus@t520.home>
%%% @copyright (C) 2014, magnus
%%% @doc
%%%
%%% @end
%%% Created : 11 Mar 2014 by magnus <magnus@t520.home>
%%%-------------------------------------------------------------------
-module(bride_telnet).

-behaviour(gen_server).
-behaviour(bride_plugin).

%% bride_plugin API
-export([start/1,
	 connect_local_server/3,
	 send_local_server/2,
	 disconnect_local_server/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(TELNET_PORT, 23).
-define(TELNET_HOST, {127,0,0,1}).
-record(st, {
	  local_socket = nil,
	  cb_ref = nil,
	  cb_pid = nil,
	  port = ?TELNET_PORT,
	  host = ?TELNET_HOST
	  
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
start(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

connect_local_server(Pid, CBPid, CBRef) ->
    gen_server:call(Pid, {connect_local_server, CBPid, CBRef}).

send_local_server(Pid, Data) ->
    gen_server:call(Pid, {send_local_server, Data}).

disconnect_local_server(Pid) ->
    gen_server:call(Pid, disconnect_local_server).


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
init(Opts) ->
    {ok, #st{ 
	    host = util:get_opt(host, Opts, ?TELNET_HOST), 
	    port = util:get_opt(port, Opts, ?TELNET_PORT)
	   }
    }.


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
handle_call({connect_local_server, CBPid, CBRef}, _From, St) ->
    io:format("bride_telnet: Connecting to local server at ~p:~p. CBPid: ~p~n",
	      [St#st.host, St#st.port, CBPid]),
    case gen_tcp:connect(St#st.host, St#st.port, [binary, { active, true}]) of
	{ ok, Socket } ->
	    io:format("bride_telnet: Connectiion established~n"),

	    {reply, ok, St#st { local_socket = Socket, 
				cb_pid = CBPid,
				cb_ref = CBRef } };

	{ error, Reason } ->
	    {reply, { error, Reason}, St }
    end;

handle_call({send_local_server, Data}, _From, St) ->
    case gen_tcp:send(St#st.local_socket, Data) of
	ok ->
	    {reply, ok, St };
	
	{ error, Reason } ->
	    St#st.cb_pid ! { bride_disconnect, St#st.cb_ref, error, Reason},
	    {stop, { error, Reason}, St#st { } }
    end;

handle_call(disconnect_local_server, _From, _St) ->
    {stop,  ok, #st {} };

handle_call(_Request, _From, St) ->
    {reply, ok, St}.

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

handle_info({tcp_closed, _Socket}, State) ->
    State#st.cb_pid ! { bride_disconnect, cb_ref, closed},
    {stop, State#st { local_socket = nil }};
    
handle_info({tcp_error, _Socket, Reason}, State) ->
    State#st.cb_pid ! { bride_disconnect, cb_ref, error, Reason},
    {stop, State#st { local_socket = nil }};

handle_info({tcp, _Socket, Data}, State) ->
    io:format("bride_telnet: Got data from telnet server: ~p. Sending to ~p~n", 
	      [ Data, State#st.cb_pid ]),
    State#st.cb_pid ! { bride_data, Data},
    {noreply, State};

%% Process data from the router client connected to the groom.
handle_info({router_server_data, Ref, Data}, State) ->
    io:format("bride_telnet: Got data from Router ~p: ~p~n", [ Ref, Data ]),
    gen_tcp:send(State#st.local_socket, Data),
    {noreply, State};
    
handle_info(Info, State) ->
    io:format("bride_telnet: Wut?[~p]~n", [ Info ]),
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
terminate(Reason, St) ->
    io:format("bride_telnet: terminate[~p]~n", [ Reason ]),
    if St#st.local_socket =/= nil ->
       gen_tcp:shutdown(St#st.local_socket, read_write),
       gen_tcp:close(St#st.local_socket);
       true -> true
    end,
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

%%%===================================================================
%%% Internal functions
%%%===================================================================
