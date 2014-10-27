-module (m_etp_protocol_fsm_sup).
-export([start_link/0]).

-behaviour(supervisor).
-export([init/1]).

-define(CHILD(I, Type,Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @private
init([]) ->
	{ok, { {one_for_one, 5, 10}, []} }.