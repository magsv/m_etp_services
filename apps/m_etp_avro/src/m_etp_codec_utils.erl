-module (m_etp_codec_utils).
-include_lib("../m_etp_store/include/m_etp_data.hrl").

-export([get_protocol_and_messagetype/1,decode_session_request2record_with_id/2,decode_json_protocol2record/1,decode_session_request2record/1,encode_open_session/1]).
-export([encode_msg_header/1,encode_error/2]).
decode_json_protocol2record(Data)->
   try
		Decoded=jiffy:decode(Data),
        lager:debug("Decoded data:~p",[Data]),
		Name=ej:get({"name"}, Decoded),
		MessageType=ej:get({"messageType"},Decoded),
		ProtocolNo=ej:get({"protocol"},Decoded),
		NameSpace=ej:get({"namespace"},Decoded),

		lager:debug("Parsing schema eavro"),
		CompiledSchema=eavro:parse_schema(Data),
		lager:debug("Result of compiled schema:~p",[CompiledSchema]),
		lager:debug("Parsed schema..."),
		FullName=erlang:iolist_to_binary([NameSpace,<<".">>, Name]),
		ETPProtocolRecord=#m_etp_protocol{
			full_name=FullName,
			name=Name,
			protocol_no=ProtocolNo,
			message_type=MessageType,
			name_space=NameSpace,
			raw_schema=Data,
			created=m_etp_utils:get_timestamp(),
			v_major="",
			v_minor="",
			revision="",
			patch="",
			role="",
			valid=true,
			compiled_schema=CompiledSchema
		},
		{ok,ETPProtocolRecord}
    catch _:Why ->
    	lager:error("Failed in decode of json:~p",[Why]),
		{error,failed_in_decode_json}
    end.

decode_session_request2record_with_id(SessionRequest,SessionId)->
	lager:debug("Decoding session request:~p",[SessionRequest]),
	{[[ProtocolData],ApplicationName],_}=SessionRequest,
    Decoded=[decode_protocol_data(X) || X <-ProtocolData],
	Return=[add_session_data(SessionId,m_etp_utils:get_timestamp(),ApplicationName,X) || X <-Decoded],
	lager:debug("Returning session request record list:~p",[Return]),
	Return.

decode_session_request2record(SessionRequest)->
    lager:debug("Decoding session request:~p",[SessionRequest]),
	{[[ProtocolData],ApplicationName],_}=SessionRequest,
    Decoded=[decode_protocol_data(X) || X <-ProtocolData],
	[add_session_data("",m_etp_utils:get_timestamp(),ApplicationName,X) || X <-Decoded].

	


decode_session_protocol(SessionProtocol)->
	lager:debug("Decoding session request protocol:~p",[SessionProtocol]),
    %[Protocol,Version,Role,Capabilities]=SessionProtocol.
    [decode_protocol_data(X) || X <-SessionProtocol].
	
	
decode_protocol_data(ProtocolData)->
	lager:debug("Decoding protocol data:~p",[ProtocolData]),
	[Protocol,Version,Role,Capabilities]=ProtocolData,
	lager:debug("Protocol:~p",[Protocol]),
	lager:debug("Version:~p",[Version]),
	lager:debug("Role:~p",[Role]),
	lager:debug("Capabilities:~p",[Capabilities]),
	[VMajor,VMinor,Revision,Patch]=Version,
	#m_etp_session_data{protocol=Protocol,
					v_major=VMajor,
					v_minor=VMinor,
					revision=Revision,
					patch=Patch,
					role=Role,
					capabilities=Capabilities
	}.


encode_version()->
	VersionSchema={avro_record,'Version',
                        [
                            {<<"major">>,int},
                            {<<"minor">>,int},
                            {<<"revision">>,int},
                            {<<"patch">>,int}
                        ]
                    },
   Result=eavro:encode(VersionSchema,[1,0,0,0]),
   lager:debug("Result of encode version:~p",[Result]),
   Result.


encode_error(ErrorCode,ErrorMessage)->
	[ErrorCode,ErrorMessage].

encode_open_session(SessionId)->
	Protocols=[

		{1,[1,0,0,0],<<"producer">>},%channeldata
		{1,[1,0,0,0],<<"consumer">>},%channeldata
		{2,[1,0,0,0],<<"producer">>},%channeldataframe
		{2,[1,0,0,0],<<"consumer">>},%channeldataframe
		{4,[1,0,0,0],<<"producer">>},%store
		{4,[1,0,0,0],<<"consumer">>}%store
	],
	[<<"M_ETP_SERVER">>,atom_to_binary(SessionId,utf8),
		[[encode_supported_protocol(X) || X <-Protocols]]
	].

encode_msg_header({Protocol,MessageType,CorrelationId,MessageId,MessageFlags})->
	[Protocol,MessageType,CorrelationId,MessageId,MessageFlags].

encode_supported_protocol({ProtocolNumber,Version,Role})->
	[
				ProtocolNumber,
				Version,
				Role,
				[]
	].

add_session_data(SessionId,Created,ApplicationName,SessionData)->
	NewData=SessionData#m_etp_session_data{application_name=ApplicationName,session_id=SessionId,created=Created},
	NewData.

get_protocol_and_messagetype(<<"Energistics.Protocol.Core.OpenSession">>)->
	{0,2}.


