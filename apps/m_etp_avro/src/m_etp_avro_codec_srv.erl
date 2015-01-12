-module (m_etp_avro_codec_srv).
-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,{messageheader}).
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
			{ok,HeaderSchema}=m_etp_protocol_proxy:get_protocol(<<"MessageHeader">>),
			CompiledSchema=HeaderSchema#m_etp_protocol.compiled_schema,
			NewState=State#state{messageheader=CompiledSchema},
			Response=decode_data({binary,Data,CompiledSchema}),
			{reply,Response,NewState};
		_ -> 
    		Response=decode_data({binary,Data,State#state.messageheader}),
    		{reply,Response,State}
    end;

handle_call({decode,binary,Data,Schema},_From,State)->
	Response=decode_data({binary,Data,Schema}),
    {reply,Response,State};
   
	

handle_call({encode,binary,Data,Schema},_From,State)->
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
