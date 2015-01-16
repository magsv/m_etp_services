-module (m_etp_avro_codec_handler).
-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([handle_post_avro/2]).



init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
	{[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
	{[
	  {{<<"application">>, <<"avro">>, []}, handle_post_avro}
	],
		Req, State}.


handle_post_avro(Req,State)->

	lager:debug("Got post.."),
	case cowboy_req:binding(fullname, Req) of
		{undefined, Req2} ->
			respond_with_body_and_code(<<"Missing protocol fullname">>,Req2,State,500);
		{FullName, Req2} ->
			 {Method, Req2} = cowboy_req:method(Req),
			 HasBody=cowboy_req:has_body(Req2),
			 process_post_body(HasBody,Method,FullName,Req2,State)
		    
    end.
   
process_post_body(true,<<"POST">>,FullName,Req,State)->
    case cowboy_req:body_qs(Req) of
		{ok,[{Data,_}],Req3}->
			lager:debug("Got avro payload. fullname:~p, data:~p",[FullName,Data]),
			process_decode({FullName,Data},Req3,State);
		{error,Reason,Req3}->
			lager:debug("Error post"),
			handle_result({error,Reason},Req3,State,500)
	end.

process_decode({<<"Energistics.Protocol.Core.OpenSession">>,Data},Req,State)->
	process_result(m_etp_avro_codec_proxy:decode({binary_protocol,Data,{0,2}}),Req,State).

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

process_result({ok,Decoded},Req,State)->
	lager:debug("Got decoded result:~p",[Decoded]),
	respond_with_body_and_code(jiffy:encode(Decoded),Req,State,200);

process_result({error,Reason},Req,State)->
	lager:debug("Got error during decode:~p",[Reason]),
	respond_with_body_and_code(<<"Failed in decode of protocol">>,Req,State,500).
