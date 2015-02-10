-module (m_etp_avro_func_srv).

-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("../m_etp_store/include/m_etp_data.hrl").


-define(SERVER, ?MODULE).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

%% @private
init(_Args) ->
	lager:info("Started m_etp_avro_func srv..."),
	{ok, undefined}.


handle_call({encode_open_session,{SessionId}}, _From, State) ->
    Data=m_etp_codec_utils:encode_open_session(SessionId),
    {reply, Data, State};


handle_call({encode_msg_header,{Protocol,MessageType,CorrelationId,MessageId,MessageFlags}}, _From, State) ->
	Data=m_etp_codec_utils:encode_msg_header({Protocol,MessageType,CorrelationId,MessageId,MessageFlags}),
	{reply, Data, State};


handle_call({encode_error,{ErrorCode,ErrorMessage}}, _From, State) ->
	Data=m_etp_codec_utils:encode_error(ErrorCode,ErrorMessage),
	{reply, Data, State};

handle_call({decode_session_request_to_record,{SessionRequest,SessionId}}, _From, State) ->
	Data=m_etp_codec_utils:decode_session_request2record_with_id(SessionRequest,SessionId),
	{reply, Data, State};

handle_call({decode_session_request_to_record,{SessionRequest}}, _From, State) ->
	Data=m_etp_codec_utils:decode_session_request2record(SessionRequest),
	{reply, Data, State};

handle_call({get_protocol_and_messageType,<<"Energistics.Protocol.Core.OpenSession">>}, _From, State) ->
	{reply, {0,2}, State};

handle_call({decode_etp_protocol2record,Data}, _From, State) ->
	Result=m_etp_codec_utils:decode_json_protocol2record(Data),
	{reply, Result, State};




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
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.