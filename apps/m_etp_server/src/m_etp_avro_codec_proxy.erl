-module (m_etp_avro_codec_proxy).

-export ([decode/1,encode/1]).


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
	