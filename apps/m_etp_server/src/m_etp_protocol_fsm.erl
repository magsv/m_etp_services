-module (m_etp_protocol_fsm).
-behaviour(gen_fsm).

-export([init/1, state_name/2, state_name/3, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state,{sessionid}).

init(SessionId) ->
	{ok, disconnected, #state{sessionid=SessionId}}.





state_name(Event, StateData) ->
    {next_state, state_name, StateData}.



state_name(Event, From, StateData) ->
    Reply = ok,
    {reply, Reply, state_name, StateData}.



handle_event(Event, StateName, StateData) ->
    {next_state, StateName, StateData}.



handle_sync_event(Event, From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.



handle_info(Info, StateName, StateData) ->
    {next_state, StateName, StateData}.



terminate(Reason, StateName, StatData) ->
    ok.



code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.



