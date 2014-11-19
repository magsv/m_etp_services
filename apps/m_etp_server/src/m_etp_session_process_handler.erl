-module (m_etp_session_process_handler).

-export ([create_session_and_broadcast/1,update_session_status_and_broadcast/2,
	update_session_request_and_broadcast/2,broadcast_data/2]).

create_session_and_broadcast(SessionId)->
   lager:info("In create session and broadcast with session id:~p",[SessionId]),
    Result=m_etp_session_proxy:create_session(SessionId),
	gproc:send({p, l, {socket_session,SessionId}}, {self(), {socket_session,SessionId}, Result}).

update_session_status_and_broadcast(SessionId,Status)->
	lager:info("In update session status and broadcast with session id:~p, status: ~p",[SessionId,Status]),
	Result=m_etp_session_proxy:update_session_status(SessionId,Status),
	lager:info("Result of update session status:~p",[Result]),
	gproc:send({p, l, {socket_session,SessionId}}, {self(), {socket_session,SessionId}, Result}).

update_session_request_and_broadcast(SessionId,SessionRequest)->
	lager:info("In update session request and broadcast with session id:~p, session_request: ~p",[SessionId,SessionRequest]),
	Result=m_etp_session_proxy:update_session_request(SessionId,SessionRequest),
	gproc:send({p, l, {socket_session,SessionId}}, {self(), {socket_session,SessionId}, Result}).


broadcast_data(SessionId,{error,Reason})->
	gproc:send({p, l, {socket_session,SessionId}}, {self(), {socket_session,SessionId}, {error,Reason}});

broadcast_data(SessionId,{ok,Data})->
	gproc:send({p, l, {socket_session,SessionId}}, {self(), {socket_session,SessionId}, {ok,Data}}).



