%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 14 Jun 2012 by magnus <magnus@feuerlabs.com>
%%%-------------------------------------------------------------------
-module(exobride).

-behaviour(gen_server).

%% API
-export([start/0,
	start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-define(SERVER, ?MODULE).
-define(BRIDE_SERVICES, services).

-record(service, {
	  name = undefined,
	  bride_mod = undefined,
	  bride_opts = undefined,
	  router_mod = undefined,
	  router_opts = udnefined,
	  sideband_mod = undefined
	 }).
	  
-record(session, {
	  key = undefined,
	  service = undefined, %% To figure out service modules, etc.
	  bride_pid = undefined,
	  router_pid = undefined
	 }).

-record(st, { 
	  services = [],
	  sessions = []
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
start(Services) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Services, []).

start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, St} |
%%                     {ok, St, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, App } = application:get_application(),
    %% { links, [
    %%   { simpe, sideband_sender_simple, [ { sieband_opt1, 1} ], 
    %%             groom_telnet, [{ room_opt1, 1 },
    %%             router_simple, [ { router_opt1, 1}] 
    %% ]}
    Services = application:get_env(App, ?BRIDE_SERVICES, []),
    {ok, #st { services = setup_services(Services) }};

init(Services) ->
    {ok, #st { services = setup_services(Services) }}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call({open, Device}, From, St) ->
%%                                   {reply, Reply, St} |
%%                                   {reply, Reply, St, Timeout} |
%%                                   {noreply, St} |
%%                                   {noreply, St, Timeout} |
%%                                   {stop, Reason, Reply, St} |
%%                                   {stop, Reason, St}
%% @end
%%--------------------------------------------------------------------
handle_call(Command, _From, St) ->
    {reply, {unknown_command, Command }, St}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, St) -> {noreply, St} |
%%                                  {noreply, St, Timeout} |
%%                                  {stop, Reason, St}
%% @end
%%--------------------------------------------------------------------

handle_cast(_Msg, St) ->
    {noreply, St}.

%% sideband receiver Got a wakeup call.
handle_info({ sideband_wakeup, _From, Service, _IP, _Port, Key }, St) ->
    
    io:format("exobride:handle_info(): sideband received wakeup: ~p -> ~p~n", 
	      [ Service, Key ]),

    %% Locate the server given the service reported by sideband
    case find_service(Service, St#st.services) of
	false ->
	    io:format("WARNING: Unknown service name: ~p~n", [Service]),
	    { noreply, St };

	#service { bride_mod = BrMod,
		   bride_opts = BrOpts,
		   router_mod = RtMod,
		   router_opts = RtOpts } ->

	    %% Bring up a naked, unconnected bride process.
	    {ok, BrPid} = BrMod:start(BrOpts),

	    %% Start the router and have it connect back to the router
	    %% server using the key received from the sideband channel
	    case RtMod:start(BrPid, Service, Key, RtOpts) of
		{ok, RtPid } ->
		    %% Fire up the local bride and have it connect to 
		    %% the local ssh/http/telnet/whatever server.
		    %% Provide the router pid as the process to forward
		    %% traffic to.
		    io:format("exogroom: Setting up router server connection between ~p: Key ~p~n", 
			      [ Service, Key ]),
		    
		    io:format("exogroom: Router ~p: Bride ~p~n", 
			      [ RtPid, BrPid ]),
		    
		    case BrMod:connect_local_server(BrPid, RtPid, Service) of
			ok ->
			    { noreply, 
			      St#st { 
				sessions = [ #session { 
						key = Key,
						service = Service,
						bride_pid = BrPid,
						router_pid = RtPid 
					       } |  St#st.sessions ]
			       } };

			BrErr ->
			    io:format("exogroom: Failed to connect to local service(~p): ~p~n", 
				      [ Service, BrErr ]),
			    %% Will stop router
			    BrMod:disconnect_local_server(BrPid), 
			    RtMod:disconnect_server(RtPid),
			    { noreply, St }
		    end;

		_ ->
		    { noreply, St }
	    end
    end;

handle_info(Info, St) ->
    io:format("handle_info: ~p ~p~n", [Info, St]),
    {noreply, St}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, St) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _St) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Command process st when code is changed
%%
%% @spec code_change(OldVsn, St, Extra) -> {ok, NewSt}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



setup_services(SvcList) ->     
    setup_services(SvcList, []).


setup_services([], Acc) ->
    Acc;

setup_services([{ Name, 
		  BrideMod, BrOpts, 
		  SidebandMod, SbOpts,
		  RouterMod, RtOpts} | T], Acc) ->

    io:format("exobride:setup_services(~p): Bride: ~p BrideOpts: ~p~n", 
	      [ Name, BrideMod, BrOpts ]),

    io:format("exobride:setup_services(~p): Sideband: ~p SidebandOpts: ~p~n", 
	      [ Name, SidebandMod, SbOpts ]),
    
    io:format("exobride:setup_services(~p): Router: ~p RouterOpts: ~p~n", 
	      [ Name, RouterMod, RtOpts ]),
    

    %% FIXME:
    %% We probably should have the bride setup its own sideband and router
    %% processes so that we don't have to run all traffic and management
    %% messages through self().
    %%
    %% Fire up the bride.

    %% Setup sideband module, which will receive the initial signal
    %% to setup a bride.
    case SidebandMod:start(self(), Name, SbOpts) of 
	ok ->
	    setup_services(T, [ #service { 	
				   name = Name, 
				   bride_mod = BrideMod,
				   bride_opts = BrOpts,
				   sideband_mod = SidebandMod,
				   router_mod = RouterMod,
				   router_opts = RtOpts
				  } | Acc ]);
	SbErr ->
	    io:format("setup_services(~p): Failed to setup sideband ~p: ~p~n", 
		      [ Name, SidebandMod, SbErr ]),
	    setup_services(T,  Acc)

    end;

setup_services([WrongFormat | T], Acc) ->
    io:format("setup_services(~p): Wrong format. Ignored.~n",
	      [ WrongFormat ]),
    io:format("                    Use { Name, BrMod, BrOpts, SbMod, SbOpts, RtMod, RtOpts }~n"),
    setup_services(T, Acc).


find_service(Name, SvcList) ->
    lists:keyfind(Name, #service.name, SvcList).
