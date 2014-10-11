-module (m_etp_socket_ping_handler).

-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).
-export([terminate/3]).
-record(state,{}).

init({tcp, http}, Req, Opts) ->
	{upgrade, protocol, cowboy_websocket}.
	
	
    
	

websocket_init(_TransportName, Req, _Opts) ->
	case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req) of
        {ok, undefined, Req2} ->
            {ok, Req, #state{}};
        {ok, Subprotocols, Req2} ->
        	
            case lists:member(<<"energistics-tp">>,Subprotocols) of
                true ->
                    Req3 = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>,
                        <<"energistics-tp">>, Req2),
                    {ok, Req3, #state{}};
                false ->
                	lager:info("Unsupported protocol found, ~p...",[Subprotocols]),
                    {shutdown, Req2}
            end
    end.
	

websocket_handle({text, Msg}, Req, State) ->
	lager:info("In handling text"),
	{reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
websocket_handle(Data, Req, State) ->
	lager:info("In handle other data:,~p",[Data]),
	{ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
	{reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.

terminate(_Reason, _Req, _State)->
	ok.