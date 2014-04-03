%%%-------------------------------------------------------------------
%%% @author magnus <magnus@t520.home>
%%% @copyright (C) 2014, magnus
%%% @doc
%%%
%%% @end
%%% Created : 11 Mar 2014 by magnus <magnus@t520.home>
%%%-------------------------------------------------------------------
-module(router_client_simple).

-behaviour(gen_server).
-behaviour(router_client_plugin).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% router_server_plugin callback
-export([start/4, 
	 send_server/2,
	 disconnect_server/1]).

-define(SERVER, ?MODULE). 
-define(PORT_DEFAULT, 4711). 
-define(HOST_DEFAULT, { 127,0,0,1}). 


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
start(CBPid, CBArg, Key, Opts) ->
    gen_server:start_link(?MODULE, {CBPid, CBArg, Key, Opts}, []).

send_server(Ref, Data) ->
    gen_server:call(Ref, {router_send_server, Data}).

disconnect_server(Ref) ->
    gen_server:call(Ref, disconnect_server).
    
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
init({CBPid, CBArg, Key, Opts}) ->
    Host = util:get_opt(router_server_address, Opts, ?HOST_DEFAULT),
    Port = util:get_opt(router_server_port, Opts, ?PORT_DEFAULT),
    case gen_tcp:connect(Host, Port, [binary, { active, true }]) of
	
	{ok, ServerSock} ->
	    %% Send over the key.
	    ok = gen_tcp:send(ServerSock, "key:"++Key),

	    {ok, #st {
			  sock = ServerSock,
			  cb_pid = CBPid,
			  cb_arg = CBArg
			 }};

	Err ->
	    { stop, Err }
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
handle_call({router_send_server, Data}, _From, St) ->
    gen_tcp:send(St#st.sock, Data),
    {reply, ok, St };    


handle_call(disconnect_server, _From, St) ->
    io:format("router_client_simple: Will disconnect router server~n"),
    case St#st.sock of 
	nil -> true;

	_ ->
	    gen_tcp:shutdown(St#st.sock, read_write),
	    gen_tcp:close(St#st.sock)
    end,
    { stop, normal, ok, #st{}};



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
handle_info( {tcp, _Socket, Data }, St) ->
    io:format("router_client_simple: Received ~p router server. Fwd to bride ~p~n",
	      [Data, St#st.cb_pid]),
    St#st.cb_pid ! { router_client_data, St#st.cb_arg, Data },
    { noreply, St };

handle_info( {tcp_closed, Socket }, St) ->
    io:format("router_client_simple: Router server disconnected. Fwd to bride ~p~n",
	      [St#st.cb_pid]),
    St#st.cb_pid ! { router_client_disconnect, St#st.cb_arg },
    gen_tcp:close(Socket),
    { stop, normal, St };

%% Receive data from the linked bride
handle_info( { bride_data,  _From, _Ref, Data}, St) ->
    io:format("router_client_simple: Received ~p from bride. Fwd to router server~n",
	      [Data]),
    gen_tcp:send(St#st.sock, Data),
    { noreply, St };

handle_info( { bride_disconnect, _From, _Ref, Reason}, St) ->
    io:format("router_client_simple: Bride disconnected: ~p. Kill socket and die.~n",
	      [Reason]),

    gen_tcp:shutdown(St#st.sock, read_write),
    gen_tcp:close(St#st.sock),
    { stop, normal, St };


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

