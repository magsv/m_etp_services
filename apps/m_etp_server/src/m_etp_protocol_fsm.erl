-module (m_etp_protocol_fsm).
-behaviour(gen_fsm).

-export([init/1, state_name/2, state_name/3, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([start_link/1,disconnected/2,connected/2,hibernating/2,in_session/2]).
-record(state,{sessionid}).

start_link(SessionId)->
    lager:info("Start link called. with args,~p",[SessionId]),
    gen_fsm:start_link({local,SessionId},?MODULE,SessionId,[]).

init(SessionId) ->
    lager:info("Init gen fsm with session id:~p",[SessionId]),
	{ok, disconnected, #state{sessionid=SessionId}}.


disconnected({connected},State)->
    lager:info("Session id:~p is connected",[State#state.sessionid]),
    lager:info("Intializing session state data"),
    spawn(m_etp_session_process_handler,create_session_and_broadcast,[State#state.sessionid]),
    {next_state,connected,State}.

%connected(Event,State)->
 %   lager:info("FSM connected,~p",[State#state.sessionid]),
 %   {next_state,connected,State};



connected({Data},State)->
    {next_state,in_session,State}.

in_session(Event,State)->
    {next_state,in_session,State};


in_session({close_session},State)->
    {next_state,disconnected,State}.

hibernating(Event,State)->
    {next_state,hibernating,State}.

state_name(Event, StateData) ->
    {next_state, state_name, StateData}.



state_name(Event, _From, StateData) ->
    Reply = ok,
    {reply, Reply, state_name, StateData}.



handle_event(Event, StateName, StateData) ->
    {next_state, StateName, StateData}.



handle_sync_event(Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.



handle_info(Info, StateName, StateData) ->
    {next_state, StateName, StateData}.



terminate(Reason, StateName, StatData) ->
    ok.



code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.



