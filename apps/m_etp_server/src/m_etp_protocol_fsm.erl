-module (m_etp_protocol_fsm).
-behaviour(gen_fsm).

-export([init/1,  handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([start_link/2,disconnected/2,connected/2,hibernating/2,in_session/2]).

-include_lib("../m_etp_store/include/m_etp_data.hrl").

-record(state,{sessionid,encoding}).

start_link(SessionId,Encoding)->
    
    gen_fsm:start_link({local,SessionId},?MODULE,{SessionId,Encoding},[]).

init({SessionId,Encoding}) ->
    lager:debug("Init gen fsm with session id:~p and serialization type:~p",[SessionId,Encoding]),
	{ok, disconnected, #state{sessionid=SessionId,encoding=Encoding}}.


disconnected({connected},State)->
    lager:debug("Session id:~p is connected",[State#state.sessionid]),
    {next_state,connected,State}.

%connected(Event,State)->
 %   lager:debug("FSM connected,~p",[State#state.sessionid]),
 %   {next_state,connected,State};

connected({hibernating},State)->
    lager:debug("FSM state move into hibernation awaiting rewake or clean,sessionId:~p",[State#state.sessionid]),
    {next_state,hibernating,State};


connected({closed},State)->
    lager:debug("FSM state move into closed awaiting possible rewake or clean"),
    {next_state,disconnected,State};



connected({RequestData},State) when is_atom(RequestData)==false -> 
    lager:debug("Got data in connected state should be request session.."),
    % try to get the request session schema..
    handle_protocol(request_session,m_etp_protocol_proxy:get_protocol(<<"RequestSession">>),State#state.encoding,RequestData,State).

session_acknowledge({ok,ValidProtocols},State)->
    {next_state,in_session,State};

session_acknowledge({error,Reason},State)->
    {next_state,in_session,State}.


in_session({close_session},State)->
    {next_state,disconnected,State};

in_session(Event,State)->
    lager:debug("Got event:~p in in_session state",[Event]),
    {next_state,in_session,State}.




hibernating(_Event,State)->
    {next_state,hibernating,State}.


handle_event({stop}, _StateName, State) ->
lager:debug("FSM with session id told to stop and shutdown,~p",[State#state.sessionid]),
{ stop, normal, State };


handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.



handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.


handle_info({'DOWN', _MonitorReference, process, _Pid, Reason},StateName,StateData) when not is_atom(Reason)->
    lager:error("Recieved down signal from spawned process:~p",[Reason]),
    %broadcast the error back to the websocket creating the error
    m_etp_session_process_handler:broadcast_data(StateData#state.sessionid,{error,m_controlling_process_faulted}),
    {next_state, StateName, StateData};


handle_info({'DOWN', _MonitorReference, process, _Pid, Reason},StateName,StateData) when is_atom(Reason)->
    lager:debug("Spawned process exited normally:~p",[Reason]),
    {next_state, StateName, StateData};


handle_info(Info, StateName, StateData) ->
    lager:debug("Recieved info event:~p",[Info]),
    {next_state, StateName, StateData}.



terminate(Reason, _StateName, State) ->
    lager:error("Protocol fsm terminated:~p",[Reason]),
    m_etp_session_process_handler:broadcast_data(State#state.sessionid,{error,protocol_fsm_error}),
    ok.



code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


handle_protocol(request_session,{ok,no_data_found},_Encoding,_RequestData,State)->
    lager:debug("Request session protocol not found"),
    spawn_monitor(m_etp_session_process_handler,broadcast_data,[State#state.sessionid,{error,no_data_found}]),
    {next_state,connected,State};

handle_protocol(request_session,{error,Reason},_Encoding,_RequestData,State)->
    lager:debug("Request session protocol not found"),
    spawn_monitor(m_etp_session_process_handler,broadcast_data,[State#state.sessionid,{error,Reason}]),

    {next_state,connected,State};

handle_protocol(request_session,{ok,Schema},binary_ocf,RequestData,State) when is_atom(Schema)==false ->
    lager:debug("Handling binary ocf decode request session"),
    Result=m_etp_avro_codec_proxy:decode({binary_ocf,RequestData}),
    process_result(Result,decode,binary_ocf,State),
    {next_state,session_acknowledge,State};


handle_protocol(request_session,{ok,Schema},binary,RequestData,State) when is_atom(Schema)==false ->
    lager:debug("Handling binary decode request session with compile schema:~p",[Schema#m_etp_protocol.compiled_schema]),
    MessageData=m_etp_avro_codec_proxy:decode({binary_message_header,RequestData}),
    lager:debug("Result of decode message data:~p",[MessageData]),
    Result=m_etp_avro_codec_proxy:decode({binary,RequestData,Schema#m_etp_protocol.compiled_schema}),
    process_result(Result,decode,binary,State),
    {next_state,session_acknowledge,State}.



process_result({error,Reason},decode,binary_ocf,State)->
    lager:error("Failed in decode:~p",[Reason]),
    spawn_monitor(m_etp_session_process_handler,broadcast_data,[State#state.sessionid,{error,failed_decode_binary_ocf}]),
    {next_state,connected,State};

process_result({error,Reason},decode,binary,State)->
    lager:error("Failed in decode:~p",[Reason]),
    spawn_monitor(m_etp_session_process_handler,broadcast_data,[State#state.sessionid,{error,failed_decode_binary}]),
    {next_state,connected,State};



process_result({ok,Decoded},decode,binary_ocf,State)->
    {_,Data}=Decoded,
    lager:debug("Decoded binary ocf data:~p",[Data]),
    spawn_monitor(m_etp_session_process_handler,store_session_data_request_and_broadcast,[State#state.sessionid,Data]),
    spawn_monitor(m_etp_session_process_handler,update_session_request_and_broadcast,[State#state.sessionid,Data]);

process_result({ok,Decoded},decode,binary,State)->
    lager:debug("Decoded binary data:~p",[Decoded]),
    DecodedRecord=m_etp_codec_utils:decode_session_request2record_with_id(Decoded,State#state.sessionid),
    spawn_monitor(m_etp_session_process_handler,store_session_data_request_and_broadcast,[State#state.sessionid,DecodedRecord]),
    spawn_monitor(m_etp_session_process_handler,update_session_request_and_broadcast,[State#state.sessionid,Decoded]).
    

