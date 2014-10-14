-module (m_etp_protocol_proxy).

-export([create_protocol/1,get_protocol/1,update_protocol/1,delete_protocol/1]).


create_protocol(ProtocolRecord)->
	poolboy:transaction(m_etp_protocol_pool, fun(Worker) ->
        					gen_server:call(Worker,{create,ProtocolRecord})
  	end).

get_protocol(ProtocolName)->
	poolboy:transaction(m_etp_protocol_pool, fun(Worker) ->
        					gen_server:call(Worker,{get,ProtocolName})
  	end).

update_protocol(ProtocolRecord)->
	poolboy:transaction(m_etp_protocol_pool, fun(Worker) ->
        					gen_server:call(Worker,{update,ProtocolRecord})
  	end).

delete_protocol(ProtocolName)->
	poolboy:transaction(m_etp_protocol_pool, fun(Worker) ->
        					gen_server:call(Worker,{delete,ProtocolName})
  	end).