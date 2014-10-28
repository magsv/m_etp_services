-module (m_etp_protocol_fsm_sup).
-export([start_link/0]).
-export([attach_session/1]).
-behaviour(supervisor).
-export([init/1]).

-define(CHILD(I, Type,Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @private

attach_session(SessionId) ->
    %case hlr:lookup_id(SessionId) of
	%{ok, _Pid}    ->
	%    {error, attached};
	%_NotAttached ->
		lager:info("Attaching new m_etp_protocol_fsm with session id,~p",[SessionId]),
	    ChildSpec = {SessionId, {m_etp_protocol_fsm, start_link, [SessionId]},
			         transient, 2000, worker, [m_etp_protocol_fsm]},
	    supervisor:start_child(?MODULE, ChildSpec).
    %end.

init([]) ->
	{ok, { {one_for_one, 5, 10}, []} }.