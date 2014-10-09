-module (m_etp_store).

-export ([start/0,stop/1]).

start() ->
    
	ok = application:start(m_etp_store).
 
stop(_State) ->
    ok.
