-module(m_etp_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/ping", m_etp_ping_handler, []},
			{"/m_etp_socket_ping",m_etp_socket_ping_handler,[]},
			{"/m_etp_socket_service",m_etp_socket_handler,[]},
			{"/m_etp_protocol_service/[:code]",m_etp_protocol_handler,[]}
			
		]}
	]),
	
	{ok, _} = cowboy:start_http(http, 100, [{port, get_server_port()}], [
		{env, [{dispatch, Dispatch}]}
	]),
    m_etp_server_sup:start_link().

stop(_State) ->
    ok.

get_server_port()->
	{ok, [{port,Port}]} = application:get_env(m_etp_server,m_etp_server_config),
	lager:info("Setting server port to:~p",[Port]),
	Port.