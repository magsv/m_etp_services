-module(m_etp_socket_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state,{handshake_done=false,
	request_session_recieved=false,sent_open
	=false,session_id}).

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
	%lager:info("Handling binary data,state:~p,~p",[Data,State]),
	{ok, Req, State};
	



websocket_handle(Data, Req, State) ->
	lager:info("Ignoring other data:~p",[Data]),
	{ok, Req, State}.

websocket_info(post_init, Req, State) ->
    SessionId=list_to_atom(m_etp_utils:get_session_token()),
    NewState=State#state{handshake_done=true,session_id=SessionId},
    lager:info("Websocket accepted with new sessionId:~p",[SessionId]),
    m_etp_protocol_fsm_sup:attach_session(SessionId),
    {ok, Req, NewState};

websocket_info({timeout, _Ref, Msg}, Req, State) ->
	{reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.