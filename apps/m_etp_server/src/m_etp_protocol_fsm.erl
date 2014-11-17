-module (m_etp_protocol_fsm).
-behaviour(gen_fsm).

-export([init/1,  handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([start_link/2,disconnected/2,connected/2,hibernating/2,in_session/2]).

-include_lib("../m_etp_store/include/m_etp_data.hrl").

-record(state,{sessionid,encoding}).

start_link(SessionId,Encoding)->
    
    gen_fsm:start_link({local,SessionId},?MODULE,{SessionId,Encoding},[]).

init({SessionId,Encoding}) ->
    lager:info("Init gen fsm with session id:~p and serialization type:~p",[SessionId,Encoding]),
	{ok, disconnected, #state{sessionid=SessionId,encoding=Encoding}}.


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


connected({closed},State)->
    lager:info("FSM state move into closed awaiting possible rewake or clean"),
    {next_state,disconnected,State};



connected({RequestData},State) when is_atom(RequestData)==false -> 
    lager:info("Got data in connected state should be request session.."),
    % try to get the request session schema..
    handle_protocol(request_session,m_etp_protocol_proxy:get_protocol(<<"RequestSession">>),State#state.encoding,RequestData,State),
   
    {next_state,in_session,State}.

in_session({close_session},State)->
    {next_state,disconnected,State};

in_session(_Event,State)->
    {next_state,in_session,State}.




hibernating(_Event,State)->
    {next_state,hibernating,State}.


handle_event({stop}, _StateName, State) ->
lager:info("FSM with session id told to stop and shutdown,~p",[State#state.sessionid]),
{ stop, normal, State };


handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.



handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.



handle_info(Info, StateName, StateData) ->
    lager:info("Recieved info event:~p",[Info]),
    {next_state, StateName, StateData}.



terminate(_Reason, _StateName, _StatData) ->
    ok.



code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


handle_protocol(request_session,{ok,no_data_found},_Encoding,_RequestData,State)->
    lager:info("Request session protocol not found"),
    spawn(m_etp_session_process_handler,broadcast_data,[State#state.sessionid,{error,no_data_found}]),
    {next_state,connected,State};

handle_protocol(request_session,{error,Reason},_Encoding,_RequestData,State)->
    lager:info("Request session protocol not found"),
    spawn(m_etp_session_process_handler,broadcast_data,[State#state.sessionid,{error,Reason}]),

    {next_state,connected,State};

handle_protocol(request_session,{ok,Schema},binary,RequestData,State) when is_atom(Schema)==false ->
    lager:info("Protocol found parsing data..."),
    Result=m_etp_avro_codec_proxy:decode({binary,RequestData}),
    process_result(Result,decode,binary,State),
    %spawn_link(m_etp_codec_avro,decode,[binary,RequestData]),
    %{SchemaParsed,OCFResult}=eavro_ocf_codec:decode(RequestData,undefined),
    %lager:info("Parsed schema:~p",[SchemaParsed]),
    %lager:info("Result of ocf:~p",[OCFResult]),

    %lager:info("Schema request data:~p",[RequestData]),
    %SchemaParsed=eavro:parse_schema(Schema#m_etp_protocol.raw_schema),
    %lager:info("Schema parsed:~p",[SchemaParsed]),
    %Result1=eavro:decode(SchemaParsed,Schema),
    %lager:info("Result of decode1:~p",[Result1]),
    %Result=eavro:decode(Schema#m_etp_protocol.compiled_schema,RequestData),
    %lager:info("Result of decode:~p",[Result]),
    {next_state,connected,State}.


process_result({error,Reason},decode,binary,State)->
    lager:info("Failed in decode:~p",[Reason]),
    spawn(m_etp_session_process_handler,broadcast_data,[State#state.sessionid,{error,failed_decode_binary}]),
    {next_state,connected,State};


process_result({ok,Decoded},decode,binary,State)->
    lager:info("Got decoded data:~p",[Decoded]),
    {next_state,in_session,State}.