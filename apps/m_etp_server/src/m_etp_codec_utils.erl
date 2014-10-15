-module (m_etp_codec_utils).
-include_lib("../m_etp_store/include/m_etp_data.hrl").

-export([decode_json_protocol2record/1]).

decode_json_protocol2record(Data)->
   try
		Decoded=jiffy:decode(Data),
		Name=ej:get({"name"}, Decoded),
		MessageType=ej:get({"messageType"},Decoded),
		ProtocolNo=ej:get({"protocol"},Decoded),
		NameSpace=ej:get({"namespace"},Decoded),
		CompiledSchema=eavro:parse_schema(Data),
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
	