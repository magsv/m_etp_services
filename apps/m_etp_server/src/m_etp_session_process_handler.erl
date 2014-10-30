-module (m_etp_session_process_handler).

-export ([create_session_and_broadcast/1]).

create_session_and_broadcast(SessionId)->
    Result=m_etp_session_proxy:create_session(SessionId),
	gproc:send({p, l, {socket_session,SessionId}}, {self(), {socket_session,SessionId}, Result}).