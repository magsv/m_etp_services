-module (m_etp_protocol_fsm).
-behaviour(gen_fsm).

-export([init/1,  handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([start_link/2,disconnected/2,connected/2,hibernating/2,in_session/2,session_acknowledge/2]).

-include_lib("../m_etp_store/include/m_etp_data.hrl").

-record(state,{sessionid,encoding,messageid}).

start_link(SessionId,Encoding)->
    
    gen_fsm:start_link({local,SessionId},?MODULE,{SessionId,Encoding},[]).

init({SessionId,Encoding}) ->
    lager:debug("Init gen fsm with session id:~p and serialization type:~p",[SessionId,Encoding]),
	{ok, disconnected, #state{sessionid=SessionId,encoding=Encoding,messageid=0}}.


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

connected({[0,5,_,_,_],_EnclosingMessage},State)->
    lager:debug("Got disconnect call, cleaning up..."),
    {stop, normal, State};

connected({[Protocol,MessageType,CorrelationId,MessageId,MessageFlags],EnclosingMessage},State) when Protocol==0,MessageType==1-> 
    lager:debug("Got data in connected state should be request session.."),
    % try to get the request session schema..
    handle_protocol({[Protocol,MessageType,CorrelationId,MessageId,MessageFlags],EnclosingMessage},State#state.encoding,State);

connected({[Protocol,MessageType,_CorrelationId,_MessageId,_MessageFlags],_EnclosingMessage},State) when Protocol/=0;MessageType/=1-> 
    lager:debug("Invalid message type recieved in connected state:Protocol,~p MessageType,~p",[Protocol,MessageType]),
    spawn_monitor(m_etp_session_process_handler,broadcast_data,[State#state.sessionid,{error,invalid_protocol_for_state}]),
    {next_state,connected,State}.


session_acknowledge({[0,5,_,_,_],_EnclosingMessage},State)->
    lager:debug("Got disconnect call, cleaning up..."),
    {stop, normal, State};

session_acknowledge({[Protocol,MessageType,_CorrelationId,_MessageId,_MessageFlags],_EnclosingMessage},State) when Protocol/=0;MessageType/=5->
    spawn_monitor(m_etp_session_process_handler,broadcast_data,[State#state.sessionid,{error,invalid_protocol_for_state}]),
    {next_state,session_acknowledge,State};


session_acknowledge({hibernating},State)->
    lager:debug("FSM state move into hibernation awaiting rewake or clean,sessionId:~p",[State#state.sessionid]),
    {next_state,hibernating,State};

session_acknowledge({ok,ValidProtocols},State)->
    {next_state,in_session,State};

session_acknowledge({error,Reason},State)->
    {next_state,in_session,State}.


in_session({[0,5,_,_,_],_EnclosingMessage},State)->
    lager:debug("Got disconnect call, cleaning up..."),
    {stop, normal, State};

in_session({hibernating},State)->
    lager:debug("FSM state move into hibernation awaiting rewake or clean,sessionId:~p",[State#state.sessionid]),
    {next_state,hibernating,State};

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


terminate(normal, _StateName, State) ->
    lager:debug("Gen fsm told to shutdown down due to close session, cleaning up..."),
    spawn(m_etp_session_process_handler,update_session_status_and_broadcast,[State#state.sessionid,disconnected]),
    ok;


terminate(Reason, _StateName, State) ->
    lager:error("Protocol fsm terminated:~p",[Reason]),
    m_etp_session_process_handler:broadcast_data(State#state.sessionid,{error,protocol_fsm_error}),
    ok.



code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.









handle_protocol({[0,1,CorrelationId,MessageId,MessageFlags],EnclosingMessage},binary,State) ->
    lager:debug("Processing request session with decoded message header and payload"),
    Result=m_etp_avro_codec_proxy:decode({binary_protocol,EnclosingMessage,{0,1}}),
    lager:debug("Result of decode payload:~p",[Result]),
    %lager:debug("Handling binary decode request session with compile schema:~p",[Schema#m_etp_protocol.compiled_schema]),
    %MessageData=m_etp_avro_codec_proxy:decode({binary_message_header,RequestData}),
    %lager:debug("Result of decode message data:~p",[MessageData]),
    %Result=m_etp_avro_codec_proxy:decode({binary,RequestData,Schema#m_etp_protocol.compiled_schema}),
    process_result({Result,decode,binary,[0,1,CorrelationId,MessageId,MessageFlags]},State).
    





process_result({{error,Reason},decode,binary,[_MsgHeader]},State)->
    lager:error("Failed in decode:~p",[Reason]),
    spawn_monitor(m_etp_session_process_handler,broadcast_data,[State#state.sessionid,{error,failed_decode_binary}]),
    {next_state,connected,State};



%process request session data
process_result({{ok,Decoded},decode,binary,[0,1,_CorrelationId,MessageId,MessageFlags]},State)->
    lager:debug("Decoded binary data:~p",[Decoded]),
    DecodedRecord=m_etp_avro_codec_proxy:decode_session_request_to_record({Decoded,State#state.sessionid}),
    %start process to store session data request
    spawn_monitor(m_etp_session_process_handler,store_session_data_request_and_broadcast,[State#state.sessionid,DecodedRecord]),
    %start process to update session request status
    spawn_monitor(m_etp_session_process_handler,update_session_request_and_broadcast,[State#state.sessionid,Decoded]),
    %encode avro opensession request response
    OpenSessionData=m_etp_avro_codec_proxy:encode_open_session({State#state.sessionid}),
    MsgHeader=m_etp_avro_codec_proxy:encode_msg_header({0,2,MessageId,State#state.messageid,MessageFlags}),
    spawn_monitor(m_etp_session_process_handler,broadcast_data_with_msg,[State#state.sessionid,m_etp_open_session,
            m_etp_avro_codec_proxy:encode({binary_protocol,OpenSessionData,
            m_etp_avro_codec_proxy:get_protocol_and_messagetype(<<"Energistics.Protocol.Core.OpenSession">>),MsgHeader})]),
    {next_state,in_session,State}.
    

