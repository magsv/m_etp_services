-module (m_etp_protocol_handler).

-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([handle_request_json/2]).
-export([handle_post_json/2]).
-export([delete_resource/2]).
-export([terminate/3]).
-include_lib("../m_etp_store/include/m_etp_data.hrl").

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
    {Method, Req2} = cowboy_req:method(Req),
	HasBody=cowboy_req:has_body(Req2),
	process_post_body(HasBody,Method,Req2,State).
	
    
handle_request_json(Req,State)->
    {Method, Req2} = cowboy_req:method(Req),
    handle_request_response(Req2,State,Method,json).
   

delete_resource(Req, State) ->  
    case cowboy_req:binding(code, Req) of
		{undefined, Req2} ->
			respond_with_body_and_code(<<"Missing protocol name">>,Req,State,500);
		{Code, Req2} ->
  			process_delete_resource(Code,Req2,State)
		    
    end.


handle_request_response(Req,State,<<"GET">>,Mode)->
	
	case cowboy_req:binding(code, Req) of
		{undefined, Req2} ->
			respond_with_body_and_code(<<"Missing protocol name">>,Req,State,500);
		{Code, Req2} ->
			QueryResult=m_etp_protocol_proxy:get_protocol(Code),
  			process_get_response(QueryResult,Mode,Req,State)
		    
    end.

process_delete_resource(Code,Req,State)->
	process_delete_response(m_etp_protocol_proxy:delete_protocol(Code),Req,State).

process_delete_response({ok,_},Req,State)->
	handle_result({ok,<<"DELETED">>},Req,State,200);

process_delete_response({error,Reason},Req,State)->
	handle_result({error,Reason},Req,State,500).

process_post_body(true,<<"POST">>,Req,State)->
    case cowboy_req:body_qs(Req) of
		{ok,[{Data,_}],Req3}->
			RecordData=m_etp_codec_utils:decode_json_protocol2record(Data),
			case RecordData of 
				{ok,ProtocolRecord}->
					process_result(m_etp_protocol_proxy:create_protocol(ProtocolRecord),Req3,State);
				{error,Reason}->
					handle_result({error,Reason},Req3,State,500)
			end;
		{error,Reason,Req3}->
			lager:info("Error post"),
			handle_result({error,Reason},Req,State,500)
	end;



process_post_body(true,<<"PUT">>,Req,State)->
    case cowboy_req:body_qs(Req) of
		{ok,[{Data,_}],Req3}->
			RecordData=m_etp_codec_utils:decode_json_protocol2record(Data),
			case RecordData of 
				{ok,ProtocolRecord}->
					process_result(m_etp_protocol_proxy:update_protocol(ProtocolRecord),Req3,State);
				{error,Reason}->
					handle_result({error,Reason},Req3,State,500)
			end;
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

terminate(_Reason, _Req, _State) ->
    ok.

process_get_response({error,Reason},Mode,Req,State)->
	handle_result({error,Reason},Req,State,500);

process_get_response({ok,Data},Mode,Req,State) when is_atom(Data)->
	handle_result({ok,atom_to_binary(Data,unicode) },Req,State,404);


process_get_response({ok,Data},Mode,Req,State) when is_atom(Data)==false->
	handle_result({ok,Data#m_etp_protocol.raw_schema},Req,State,200).

process_result({ok,Data},Req,State)->
	handle_result({ok,<<"ok">>},Req,State,201);

process_result({error,Reason},Req,State)->
	handle_result({error,Reason},Req,State,500).
