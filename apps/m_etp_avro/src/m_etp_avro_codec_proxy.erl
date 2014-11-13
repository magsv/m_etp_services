-module (m_etp_avro_codec_proxy).

-export ([decode/1]).


decode({binary,Data})->
	poolboy:transaction(m_etp_avro_codec_pool, fun(Worker) ->
        					gen_server:call(Worker,{decode,binary,Data})
  	end).
	