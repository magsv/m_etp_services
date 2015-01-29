-module(m_etp_socket_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state,{session_id,session_stored,sesstion_request_stored}).

-define(WSKey,{pubsub,wsbroadcast}).

init({tcp, http}, _Req, _Opts) ->
    
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
   
	case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req) of
        {ok, undefined, Req2} ->
            
            {ok, Req2, #state{}};
        {ok, Subprotocols, Req2} ->
        	lager:debug("Processing protocols"),
            case lists:member(<<"energistics-tp">>,Subprotocols) of
                true ->
                    %check if we have an encoding defined
                    lager:debug("Processing encodings"),
                    {HasEncoding,_}=cowboy_req:header(<<"etp-encoding">>, Req2,undefined),
                    case HasEncoding of 
                        <<"binary">> ->
                            handle_init_encoding({ok,binary},Req2);
                        <<"binary_ocf">> ->
                            handle_init_encoding({ok,binary_ocf},Req2);
                        undefined->
                            handle_init_encoding({ok,binary},Req2);
                        _ ->
                            handle_init_encoding({error,HasEncoding},Req2)
                    end;

                false ->
                	lager:debug("Unsupported protocol found, ~p...",[Subprotocols]),
                    {shutdown, Req2}
            end
    end.

handle_init_encoding({ok,Encoding},Req)->
    	Req3 = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>,
                        <<"energistics-tp">>, Req),
        self() ! {post_init,Encoding},
        {ok, Req3, #state{}};

handle_init_encoding({error,Encoding},Req)->
        lager:error("Invalid encoding specified:~p, shutting down",[Encoding]),
        {shutdown, Req}.

websocket_handle({text, Msg}, Req, State) ->
	lager:debug("Handling test"),
	{reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};

websocket_handle({binary,Data},Req,State)->
	lager:debug("Handling binary data..."),
    self() ! {decode_payload,m_etp_avro_codec_proxy:decode({binary_message_header,Data})},
    {ok, Req, State};
	



websocket_handle(Data, Req, State) ->
	lager:debug("Ignoring other data:~p",[Data]),
	{ok, Req, State}.

websocket_info({decode_payload,{ok,Payload}},Req,State)->
    lager:debug("Decoded message payload:~p",[Payload]),
    gen_fsm:send_event(State#state.session_id,Payload),
    {ok,Req,State};

websocket_info({decode_payload,{error,Reason}},Req,State)->
    lager:error("Failed in decode of payload:~p",[Reason]),
    {ok,Req,State};

websocket_info({post_init,Encoding}, Req, State) ->
    SessionId=list_to_atom(m_etp_utils:get_session_token()),
    NewState=State#state{session_id=SessionId},
    %create the session and ensure it is propagated
    case m_etp_session_proxy:create_session(NewState#state.session_id) of 
        {error,_} -> 
            {shutdown,Req,State};
        {ok,_} -> 
            lager:debug("Websocket accepted with new sessionId:~p",[SessionId]),
            m_etp_protocol_fsm_sup:attach_session(SessionId,Encoding),
            gproc:reg({p, l, {socket_session,SessionId}}),
            gen_fsm:send_event(SessionId,{connected}),
            {ok, Req, NewState}
    end;

websocket_info({_PID,{socket_session,_},{ok,{m_etp_session,StoredSession,_,_,_,_,_}}},Req,State)->
    lager:debug("Got ok session created and stored:~p,",[StoredSession]),
    {ok,Req,State};

websocket_info({_PID,{socket_session,_},{ok,session_data_request_stored}},Req,State)->
    lager:debug("Got ok session data created and stored"),
    %{reply, {text, <<"Everything is ok">>}, Req, State};
    {ok,Req,State};

websocket_info({_PID,{socket_session,_SessionId},{error,Msg}},Req,State)->
    lager:debug("Got message broadcast error:~p,",[Msg]),
    %need to respond with an error message
    {shutdown, Req, State};



%handle incoming binary avro response data to write to socket
websocket_info({_PID,{socket_session,_SessionId},{error,_,Reason}},Req,State)->
    lager:debug("Got message broadcast error:~p,",[Reason]),
    %need to respond with an error message
    {shutdown, Req, State};

%write ok binary avro data to socket
websocket_info({_PID,{socket_session,_SessionId},{ok,StateMode,Data}},Req,State)->
    lager:debug("Got avro data in statemode:~p,",[StateMode]),
    %just write it out
    %file:write_file("/media/magnus/hdd_1/projects/erlang/energistics/test_avro/opensession_encode_srv_with_header.avro",Data),
    {reply, {binary, Data}, Req, State};


websocket_info({timeout, _Ref, Msg}, Req, State) ->
	{reply, {text, Msg}, Req, State};

websocket_info(Info, Req, State) ->
    lager:debug("Got unknow req msg:~p",[Info]),
	{ok, Req, State}.

websocket_terminate({remote,_,_},_Req,State)->
    lager:debug("Remote peer closed session"),
    spawn(m_etp_session_process_handler,update_session_status_and_broadcast,[State#state.session_id,hibernating]),
    gen_fsm:send_event(State#state.session_id,{hibernating});

websocket_terminate({normal,shutdown},_Req,State)->
    lager:debug("Server peer closed session,cleaning up"),
    spawn(m_etp_session_proxy,delete_session,[State#state.session_id]),
    gen_fsm:send_all_state_event(State#state.session_id,{stop}),
    timer:sleep(100), 
    m_etp_protocol_fsm_sup:remove_session(State#state.session_id);

websocket_terminate(Reason, _Req, State) ->

    lager:debug("Socket terminated for session id:~p, reason:~p",[State#state.session_id,Reason]),
    %broadcast status change and move fsm over into hibernating state
    spawn(m_etp_session_process_handler,update_session_status_and_broadcast,[State#state.session_id,hibernating]),
    gen_fsm:send_event(State#state.session_id,{hibernating}),
	ok.


