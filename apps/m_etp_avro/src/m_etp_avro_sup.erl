-module(m_etp_avro_sup).

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

init([]) ->
    Processes=[get_avro_codec_pool()],
    {ok, { {one_for_one, 5, 10}, Processes} }.


get_avro_codec_pool()->
	{ok, PoolCnfgArgs} = application:get_env(m_etp_avro, m_etp_avro_codec_pool),
	[{pool_cnfg,PoolCnfg},
	 {worker_args,[WorkerArgs]}]=PoolCnfgArgs,
	PoolArgs=[{name, {local, m_etp_avro_codec_pool}},
			  {worker_module, m_etp_avro_codec_srv}]++PoolCnfg,
	Result=poolboy:child_spec(m_etp_avro_codec_pool, PoolArgs, WorkerArgs),
	Result.

