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



connected({[Protocol,MessageType,_CorrelationId,MessageId,_MessageFlags],EnclosingMessage},State) when Protocol==0,MessageType==1-> 
    lager:debug("Got data in connected state should be request session.."),
    % try to get the request session schema..
    handle_protocol({Protocol,MessageType,MessageId,EnclosingMessage},State#state.encoding,State);

connected({[Protocol,MessageType,_CorrelationId,_MessageId,_MessageFlags],_EnclosingMessage},State) when Protocol/=0;MessageType/=1-> 
    lager:debug("Invalid message type recieved in connected state:Protocol,~p MessageType,~p",[Protocol,MessageType]),
    spawn_monitor(m_etp_session_process_handler,broadcast_data,[State#state.sessionid,{error,invalid_protocol_for_state}]).


session_acknowledge({[Protocol,MessageType,_CorrelationId,MessageId,_MessageFlags],EnclosingMessage},State)->
    spawn_monitor(m_etp_session_process_handler,broadcast_data,[State#state.sessionid,{error,invalid_protocol_for_state}]),
    {next_state,session_acknowledge,State};


session_acknowledge({hibernating},State)->
    lager:debug("FSM state move into hibernation awaiting rewake or clean,sessionId:~p",[State#state.sessionid]),
    {next_state,hibernating,State};

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









handle_protocol({0,1,MessageId,EnclosingMessage},binary,State) ->
    lager:debug("Processing request session with decoded message header and payload"),
    Result=m_etp_avro_codec_proxy:decode({binary_protocol,EnclosingMessage,{0,1}}),
    lager:debug("Result of decode payload:~p",[Result]),
    %lager:debug("Handling binary decode request session with compile schema:~p",[Schema#m_etp_protocol.compiled_schema]),
    %MessageData=m_etp_avro_codec_proxy:decode({binary_message_header,RequestData}),
    %lager:debug("Result of decode message data:~p",[MessageData]),
    %Result=m_etp_avro_codec_proxy:decode({binary,RequestData,Schema#m_etp_protocol.compiled_schema}),
    process_result({Result,decode,binary,request_session,MessageId},State),
    {next_state,session_acknowledge,State}.





process_result({{error,Reason},decode,binary,_Type,_MessageId},State)->
    lager:error("Failed in decode:~p",[Reason]),
    spawn_monitor(m_etp_session_process_handler,broadcast_data,[State#state.sessionid,{error,failed_decode_binary}]),
    {next_state,connected,State};




process_result({{ok,Decoded},decode,binary,request_session,MessageId},State)->
    lager:debug("Decoded binary data:~p",[Decoded]),
    DecodedRecord=m_etp_codec_utils:decode_session_request2record_with_id(Decoded,State#state.sessionid),
    spawn_monitor(m_etp_session_process_handler,store_session_data_request_and_broadcast,[State#state.sessionid,DecodedRecord]),
    spawn_monitor(m_etp_session_process_handler,update_session_request_and_broadcast,[State#state.sessionid,Decoded]).
    

