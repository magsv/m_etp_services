-module(m_etp_socket_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state,{session_id}).

-define(WSKey,{pubsub,wsbroadcast}).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
	case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req) of
        {ok, undefined, Req2} ->
            {ok, Req2, #state{}};
        {ok, Subprotocols, Req2} ->
        	
            case lists:member(<<"energistics-tp">>,Subprotocols) of
                true ->
                    Req3 = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>,
                        <<"energistics-tp">>, Req2),

                    self() ! post_init,
                    {ok, Req3, #state{}};
                false ->
                	lager:info("Unsupported protocol found, ~p...",[Subprotocols]),
                    {shutdown, Req2}
            end
    end.
	

websocket_handle({text, Msg}, Req, State) ->
	lager:info("Handling test"),
	{reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};

websocket_handle({binary,Data},Req,State)->
	lager:info("Handling binary data..."),
    gen_fsm:send_event(State#state.session_id,{Data}),
	{ok, Req, State};
	



websocket_handle(Data, Req, State) ->
	lager:info("Ignoring other data:~p",[Data]),
	{ok, Req, State}.

websocket_info(post_init, Req, State) ->
    SessionId=list_to_atom(m_etp_utils:get_session_token()),
    NewState=State#state{session_id=SessionId},
    lager:info("Websocket accepted with new sessionId:~p",[SessionId]),
    m_etp_protocol_fsm_sup:attach_session(SessionId),
    gproc:reg({p, l, {socket_session,SessionId}}),
    gen_fsm:send_event(SessionId,{connected}),

    {ok, Req, NewState};

websocket_info({_PID,{socket_session,_},{ok,{m_etp_session,StoredSession,_,_,_,_,_}}},Req,State)->
    lager:info("Got ok session created and stored:~p,",[StoredSession]),
    {ok,Req,State};

websocket_info({_PID,{socket_session,_SessionId},{error,Msg}},Req,State)->
    lager:info("Got message broadcast error:~p,",[Msg]),
    %need to respond with an error message
    {shutdown, Req, State};


websocket_info({timeout, _Ref, Msg}, Req, State) ->
	{reply, {text, Msg}, Req, State};

websocket_info(Info, Req, State) ->
    lager:info("Got unknow req msg:~p",[Info]),
	{ok, Req, State}.

websocket_terminate({remote,_,_},_Req,State)->
    lager:info("Remote peer closed session"),
    spawn(m_etp_session_process_handler,update_session_status_and_broadcast,[State#state.session_id,hibernating]),
    gen_fsm:send_event(State#state.session_id,{hibernating});

websocket_terminate({normal,shutdown},_Req,State)->
    lager:info("Server peer closed session,cleaning up"),
    spawn(m_etp_session_proxy,delete_session,[State#state.session_id]),
    gen_fsm:send_all_state_event(State#state.session_id,{stop}),
    timer:sleep(100), 
    m_etp_protocol_fsm_sup:remove_session(State#state.session_id);

websocket_terminate(Reason, _Req, State) ->

    lager:info("Socket terminated for session id:~p, reason:~p",[State#state.session_id,Reason]),
    %broadcast status change and move fsm over into hibernating state

    spawn(m_etp_session_process_handler,update_session_status_and_broadcast,[State#state.session_id,hibernating]),
    gen_fsm:send_event(State#state.session_id,{hibernating}),
	ok.