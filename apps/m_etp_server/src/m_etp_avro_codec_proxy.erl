-module (m_etp_avro_codec_proxy).

-export ([decode/1,encode/1,get_protocol_and_messagetype/1,decode_session_request_to_record/1]).
-export([encode_msg_header/1,decode_etp_json_protocol/1,encode_open_session/1]).

decode({binary,Data,Schema})->
	poolboy:transaction(m_etp_avro_codec_pool, fun(Worker) ->
        					gen_server:call(Worker,{decode,binary,Data,Schema})
  	end);

decode({binary_message_header,Data})->
	poolboy:transaction(m_etp_avro_codec_pool, fun(Worker) ->
        					gen_server:call(Worker,{decode,binary_message_header,Data})
  	end);

decode({binary_protocol,Data,{Protocol,MessageType}})->
  poolboy:transaction(m_etp_avro_codec_pool, fun(Worker) ->
                  gen_server:call(Worker,{decode,binary_protocol,Data,{Protocol,MessageType}})
    end);


decode({binary_ocf,Data})->
	poolboy:transaction(m_etp_avro_codec_pool, fun(Worker) ->
        					gen_server:call(Worker,{decode,binary_ocf,Data})
  	end).

encode({binary_protocol,Payload,{Protocol,MessageType},MsgHeader})->
  poolboy:transaction(m_etp_avro_codec_pool, fun(Worker) ->
                  gen_server:call(Worker,{encode,binary_protocol,Payload,{Protocol,MessageType},MsgHeader})
    end).


get_protocol_and_messagetype(Protocol)->
  poolboy:transaction(m_etp_avro_func_pool, fun(Worker) ->
                  gen_server:call(Worker,{get_protocol_and_messageType,Protocol})
    end).

decode_session_request_to_record({SessionRequest,SessionId})->
  poolboy:transaction(m_etp_avro_func_pool, fun(Worker) ->
                  gen_server:call(Worker,{decode_session_request_to_record,{SessionRequest,SessionId}})
    end).

encode_open_session({SessionId})->
  poolboy:transaction(m_etp_avro_func_pool, fun(Worker) ->
                  gen_server:call(Worker,{encode_open_session,{SessionId}})
    end).

encode_msg_header({Protocol,MessageType,CorrelationId,MessageId,MessageFlags})->
  poolboy:transaction(m_etp_avro_func_pool, fun(Worker) ->
                  gen_server:call(Worker,{encode_msg_header,{Protocol,MessageType,CorrelationId,MessageId,MessageFlags}})
    end).

decode_etp_json_protocol(Protocol)->
  poolboy:transaction(m_etp_avro_func_pool, fun(Worker) ->
                  gen_server:call(Worker,{decode_etp_protocol2record,Protocol})
    end).
	