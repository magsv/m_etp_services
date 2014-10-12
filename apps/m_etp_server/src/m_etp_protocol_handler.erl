-module (m_etp_protocol_handler).

-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([handle_request_json/2]).
-export([handle_post_json/2]).
-export([delete_resource/2]).
-export([terminate/3]).

init(_Transport, _Req, []) ->
	lager:info("Handling rest"),
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>,<<"PUT">>,<<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
     {[
         {{<<"application">>, <<"json">>, []}, handle_request_json}
     ], Req, State}.


content_types_accepted(Req, State) ->
	{[
	  {{<<"application">>, <<"json">>, []}, handle_post_json}
	],
		Req, State}.


handle_post_json(Req,State)->
	lager:info("Handling post json"),
    {Method, Req2} = cowboy_req:method(Req),
	HasBody=cowboy_req:has_body(Req2),
	process_post_body(HasBody,Method,Req2,State).
	
    
handle_request_json(Req,State)->
	lager:info("Handling get json"),
    {Method, Req2} = cowboy_req:method(Req),
   %handle_request_response(Req,State,Method,json).
   ok.

delete_resource(Req, State) ->  
    %handle_delete_request(Req,State,"USER").
    ok.

process_post_body(true,<<"POST">>,Req,State)->
    case cowboy_req:body_qs(Req) of
		{ok,[{Data,Status}],Req3}->
			RecordData=m_etp_codec_utils:decode_json_protocol2record(Data);
		{error,Reason,Req3}->
			lager:info("Error post"),
			handle_result({error,Reason},Req,State,500)
	end;

process_post_body(false,Method,Req,State)->
	lager:info("Processing body no body data found.."),
	respond_with_body_and_code(<<"Missing body">>,Req,State,500).

respond_with_body_and_code(Body,Req,State,StatusCode)->
	Resp = cowboy_req:set_resp_body(Body, Req),
  	{ok, Resp3} = cowboy_req:reply(StatusCode, Resp),
  	{halt, Resp3, State}.

handle_result(Body,Req,State,HTTPCode)->
	case Body of
		{ok,Data}->
			respond_with_body_and_code(Data,Req,State,HTTPCode);
		{error,Term} when is_atom(Term)->
			respond_with_body_and_code(atom_to_binary(Term,unicode),Req,State,HTTPCode);
		{error,Term} ->
			respond_with_body_and_code(Term,Req,State,HTTPCode)
	end.

terminate(_Reason, Req, State) ->
    ok.
