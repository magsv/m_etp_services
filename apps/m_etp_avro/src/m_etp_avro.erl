-module (m_etp_avro).

-export ([start/0,stop/1]).

start() ->
   
	ok = application:start(m_etp_avro).

stop(_State) ->
    ok.