-module (m_etp_protocol_fsm).

-record(state,{sessionid}).

init(SessionId) ->

  {ok, disconnected, #state{sessionid=SessionId}}.