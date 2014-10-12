-module (m_etp_codec_utils).

-export([decode_json_protocol2record/1]).

decode_json_protocol2record(Data)->
	Decoded=jiffy:decode(Data),
	lager:info("Decoded json data:~p",[Decoded]),
	ok.