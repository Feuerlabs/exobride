BRIDE SIDE

application:start(sasl).

%% Start as gen server, not full app. Minor difference.


%% simple is the name of the service that ties groom, routing protocol, and
%% sideband sender together.
%% 
%% bride_telnet connects to the local telnet server
%% sends the traffic to the router client (router_client_simple) to
%% be forwaded to the groom's router server.
%%
%% sideband_receiver_simple receives a wakeup from the groom, triggering
%% its bride router_client_simple server to connect back in to the 
%% groom router server. A key transmitted with the wakeup call is
%% used by the bride as a simple identification toward the groom router server.
%% 
%% router_client_simple connects to the server's router_server_simple when 
%% triggered by the wakeup call above. Any traffic received by the router client is
%% forwarded to bride_telnet to be forwarded to the telnet server.
%%

exobride:start([{ simple, bride_telnet, [ {host, {127,0,0,1}}, {port, 5555 } ], sideband_receiver_simple, [], router_client_simple, [ {router_server_address, { 127,0,0,1 }}, { router_server_port, 4711 }]}]).


Simulate a telnet server that the bride_telnet module will connect to
$ nc -l 5555
