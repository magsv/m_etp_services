-module (m_etp_session_data_proxy).

-export ([create_session_data/1,delete_session_data/1]).


create_session_data(SessionDataRecord)->
	poolboy:transaction(m_etp_session_data_pool, fun(Worker) ->
        					gen_server:call(Worker,{create_session_data,SessionDataRecord})
  	end).


delete_session_data(SessionId)->
	poolboy:transaction(m_etp_session_data_pool, fun(Worker) ->
        					gen_server:call(Worker,{delete_session_data,SessionId})
  	end).
