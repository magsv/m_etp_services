-module (m_etp_server).

-export ([start/0,stop/1]).

start() ->
    ok =application:start(ranch),
    ok =application:start(cowlib),
    ok =application:start(cowboy),
    ok =application:start(gproc),
    ok =application:start(mnesia),
	ok = application:start(m_etp_server).

stop(_State) ->
    ok.