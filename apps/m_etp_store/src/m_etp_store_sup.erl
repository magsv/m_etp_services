-module(m_etp_store_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type,Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

get_m_etp_session_time()->
	{ok, TimeCnfgArgs} = application:get_env(m_etp_store, m_etp_session_timeout_server),
	[{worker_args,WorkerArgs}]=TimeCnfgArgs,
	?CHILD(m_etp_session_time_server, worker,[WorkerArgs]).

get_m_etp_session_ghost_time()->
	{ok, TimeCnfgArgs} = application:get_env(m_etp_store, m_etp_session_ghost_server),
	[{worker_args,WorkerArgs}]=TimeCnfgArgs,
	?CHILD(m_etp_session_ghost_server, worker,[WorkerArgs]).

get_m_etp_session_pool()->
	{ok, SessionCnfgArgs} = application:get_env(m_etp_store, m_etp_session_pool_config),
	[{pool_cnfg,SessionCnfg},
	 {worker_args,[WorkerArgs]}]=SessionCnfgArgs,
	PoolArgs=[{name, {local, m_etp_store_proxy_pool}},
			  {worker_module, m_etp_session_store_server}]++SessionCnfg,
	Result=poolboy:child_spec(m_etp_store_proxy_pool, PoolArgs, WorkerArgs),
	Result.

get_m_etp_protocol_pool()->
	{ok, ProtocolCnfgArgs} = application:get_env(m_etp_store, m_etp_protocol_pool_config),
	[{pool_cnfg,ProtocolCnfg},
	 {worker_args,[WorkerArgs]}]=ProtocolCnfgArgs,
	PoolArgs=[{name, {local, m_etp_protocol_pool}},
			  {worker_module, m_etp_protocol_store_server}]++ProtocolCnfg,
	Result=poolboy:child_spec(m_etp_protocol_pool, PoolArgs, WorkerArgs),
	Result.

get_m_etp_session_data_pool()->
	{ok, SessionDataCnfgArgs} = application:get_env(m_etp_store, m_etp_session_data_pool_config),
	[{pool_cnfg,SessionDataCnfg},
	 {worker_args,[WorkerArgs]}]=SessionDataCnfgArgs,
	PoolArgs=[{name, {local, m_etp_session_data_pool}},
			  {worker_module, m_etp_session_data_store_srv}]++SessionDataCnfg,
	Result=poolboy:child_spec(m_etp_session_data_pool, PoolArgs, WorkerArgs),
	Result.

get_m_etp_valid_protocol_pool()->
	{ok, PoolCnfgArgs} = application:get_env(m_etp_store, m_etp_valid_protocol_pool_config),
	[{pool_cnfg,PoolDataCnfg},
	 {worker_args,[WorkerArgs]}]=PoolCnfgArgs,
	PoolArgs=[{name, {local, m_etp_valid_protocol_pool}},
			  {worker_module, m_etp_valid_protocol_store_srv}]++PoolDataCnfg,
	Result=poolboy:child_spec(m_etp_valid_protocol_pool, PoolArgs, WorkerArgs),
	Result.

init([]) ->
    
	Processes=[get_m_etp_session_pool(),get_m_etp_protocol_pool(),get_m_etp_session_time(),get_m_etp_session_data_pool(),get_m_etp_valid_protocol_pool(),get_m_etp_session_ghost_time()],
    {ok, { {one_for_one, 5, 10}, Processes} }.

