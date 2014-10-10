-module(m_etp_store_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

get_m_etp_session_pool()->
	{ok, SessionCnfgArgs} = application:get_env(m_etp_store, m_etp_session_pool_config),
	[{pool_cnfg,SessionCnfg},
	 {worker_args,[WorkerArgs]}]=SessionCnfgArgs,
	PoolArgs=[{name, {local, m_etp_store_proxy_pool}},
			  {worker_module, m_etp_session_store_server}]++SessionCnfg,
	Result=poolboy:child_spec(m_etp_store_proxy_pool, PoolArgs, WorkerArgs),
	Result.


init([]) ->
	Processes=[get_m_etp_session_pool()],
    {ok, { {one_for_one, 5, 10}, Processes} }.

