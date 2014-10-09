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
			{"/m_etp_socket",m_etp_socket_handler,[]}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]}
	]),
    m_etp_server_sup:start_link().

stop(_State) ->
    ok.