-module (m_etp_avro_codec_srv).
-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,{messageheader,requestsession}).
-include_lib("../m_etp_store/include/m_etp_data.hrl").

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

%% @private
init(_Args) ->
	lager:info("Started m_etp_avro_coded srv instance..."),
	{ok, #state{}}.



handle_call({decode,binary_ocf,Data},_From,State)->
	Response=decode_data({binary_ocf,Data}),
	{reply,Response,State};

handle_call({encode,binary_ocf,Data},_From,State)->
	{reply,{ok},State};

handle_call({decode,binary_message_header,Data},_From,State)->
	case State#state.messageheader of 
		undefined -> 
			CompiledSchema=get_msg_header_schema(State),
			NewState=State#state{messageheader=CompiledSchema},
			Response=decode_data({binary,Data,CompiledSchema}),
			{reply,Response,NewState};
		_ -> 
    		Response=decode_data({binary,Data,State#state.messageheader}),
    		{reply,Response,State}
    end;




 
handle_call({decode,binary_protocol,Payload,{0,1}},_From,State)->
	%decode request session protocol
	process_decode(m_etp_protocol_proxy:get_protocol(<<"Energistics.Protocol.Core.RequestSession">>),Payload,State);

handle_call({decode,binary_protocol,Payload,{0,2}},_From,State)->
	%decode open session protocol
	process_decode(m_etp_protocol_proxy:get_protocol(<<"Energistics.Protocol.Core.OpenSession">>),Payload,State);





handle_call({decode,binary,Data,Schema},_From,State)->
	Response=decode_data({binary,Data,Schema}),
    {reply,Response,State};
   
handle_call({encode,binary_protocol,Payload,{0,2},MsgHeader},_From,State)->
	lager:debug("In encode opensession:~p",[Payload]),
	process_encode(m_etp_protocol_proxy:get_protocol(<<"Energistics.Protocol.Core.OpenSession">>),Payload,MsgHeader,State);

handle_call({encode,binary,_Data,_Schema},_From,State)->
	{reply,{ok},State};




%% @private
handle_call(_Request, _From, State) ->
	{reply, {error, unknown_call}, State}.

%% @private
handle_cast(_Msg, State) ->
	{noreply, State}.

%% @private
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
terminate(Reason, _State) ->
	lager:error("Avro codec server terminate with reason:~p",[Reason]),
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


get_msg_header_schema(State)->
	case State#state.messageheader of 
		undefined -> 
			{ok,HeaderSchema}=m_etp_protocol_proxy:get_protocol(<<"Energistics.Datatypes.MessageHeader">>),
			HeaderSchema#m_etp_protocol.compiled_schema;
		_->
		 State#state.messageheader
    end.

process_decode({ok,no_data_found},_Payload,State)->
	{reply,{error,unable_to_locate_requested_protocol},State};

process_decode({ok,Schema},Payload,State) when is_atom(Schema)==false->
	{reply,decode_data({binary,Payload,Schema#m_etp_protocol.compiled_schema}),State}.


process_encode({ok,no_data_found},_Payload,_MsgHeader,State)->
	{reply,{error,unable_to_locate_requested_protocol},State};

process_encode({ok,Schema},Payload,MsgHeader,State) when is_atom(Schema)==false->
	PayloadRes=encode_data({binary,Payload,Schema#m_etp_protocol.compiled_schema}),

	{reply,encode_header(PayloadRes,MsgHeader,State),State}.


decode_data({binary,Data,Schema})->
	
    try eavro:decode(Schema,Data) of 
		Result ->
			{ok,Result}
	catch 
		_:Reason ->
			{error,Reason}
		
	end;

decode_data({binary_ocf,Data})->
	try eavro_ocf_codec:decode(Data,undefined) of 
		Result ->
			{ok,Result}
	catch 
		_:Reason ->
			{error,Reason}
		
	end.

encode_header({ok,PayLoad},MsgHeader,State)->
	HeaderSchema=get_msg_header_schema(State),
	PayLoadSchema=[HeaderSchema,bytes],
	{_,EncodedHeader}=encode_data({binary,MsgHeader,HeaderSchema}),
	lager:debug("Result of encoded header:~p",[EncodedHeader]),
	%create one united message with concatenation to be used with avros datumreader
	{ok,<<EncodedHeader/binary,PayLoad/binary>>};
	%encode_data({binary,[MsgHeader,PayLoad],PayLoadSchema});

encode_header({error,Reason},_MsgHeader,_State)->
	{error,Reason}.

encode_data({binary,Data,Schema})->
	lager:debug("Encode data:~p",[Data]),
	lager:debug("Encode data with schema:~p",[Schema]),
    try eavro:encode(Schema,Data) of 
		Result ->
			{ok,Result}
	catch 
		_:Reason ->
			lager:error("Error encode:~p",[Reason]),
			{error,m_etp_failed_encode_to_avro}
		
	end.
