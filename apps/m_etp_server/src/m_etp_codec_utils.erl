-module (m_etp_codec_utils).
-include_lib("../m_etp_store/include/m_etp_data.hrl").

-export([decode_session_request2record_with_id/2,decode_json_protocol2record/1,decode_session_request2record/1]).

decode_json_protocol2record(Data)->
   try
		Decoded=jiffy:decode(Data),
		Name=ej:get({"name"}, Decoded),
		MessageType=ej:get({"messageType"},Decoded),
		ProtocolNo=ej:get({"protocol"},Decoded),
		NameSpace=ej:get({"namespace"},Decoded),
		CompiledSchema=eavro:parse_schema(Data),
		lager:debug("Parsed schema to:~p",[CompiledSchema]),
		ETPProtocolRecord=#m_etp_protocol{
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

add_session_data(SessionId,Created,ApplicationName,SessionData)->
	NewData=SessionData#m_etp_session_data{application_name=ApplicationName,session_id=SessionId,created=Created},
	NewData.