-module (m_etp_session_proxy).

-export ([create_session/1,get_session/1,delete_session/1,update_session/2]).

create_session(SessionId)->
	poolboy:transaction(m_etp_store_proxy_pool, fun(Worker) ->
        					gen_server:call(Worker,{create_session,SessionId})
  	end).

get_session(SessionId)->
	poolboy:transaction(m_etp_store_proxy_pool, fun(Worker) ->
        					gen_server:call(Worker,{get_session,SessionId})
  	end).

delete_session(SessionId)->
	poolboy:transaction(m_etp_store_proxy_pool, fun(Worker) ->
        					gen_server:call(Worker,{destroy_session,SessionId})
  	end).


update_session(SessionId,SessionData)->
	poolboy:transaction(m_etp_store_proxy_pool, fun(Worker) ->
        					gen_server:call(Worker,{update_session,SessionId,SessionData})
  	end).

	