-module (m_etp_protocol_fsm).
-behaviour(gen_fsm).

-export([init/1,  handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
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

connected({hibernating},State)->
    lager:info("FSM state move into hibernation awaiting rewake or clean,sessionId:~p",[State#state.sessionid]),
    {next_state,hibernating,State};


connected({Data},State) when is_atom(Data)==false -> 
    lager:info("Got data in connected state should be request session.."),
    % try to get the request session schema..
    Result=m_etp_protocol_proxy:get_protocol("RequestSession"),
    lager:info("Result of fetching request session protocol:~p",[Result]),
    {next_state,in_session,State}.

in_session({close_session},State)->
    {next_state,disconnected,State};

in_session(_Event,State)->
    {next_state,in_session,State}.




hibernating(_Event,State)->
    {next_state,hibernating,State}.





handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.



handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.



handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.



terminate(Reason, StateName, StatData) ->
    ok.



code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.


handle_protocol({request_session,ok,no_data_found},State)->
    lager:info("Request session protocol not found"),
    {next_state,connected,State};

handle_protocol({request_session,error,Reason},State)->
    lager:info("Request session protocol not found"),
    {next_state,connected,State};

handle_protocol({request_session,ok,Data},State) when is_atom(Data)==false ->
    {next_state,connected,State}.
