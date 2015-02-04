-module (m_etp_session_ghost_server).

-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("../m_etp_store/include/m_etp_data.hrl").


-define(SERVER, ?MODULE).


-record(state,{ghost_timeout,clean_interval,tref}).


start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

%% @private
init([{ghost_timeout,Timeout},{session_clean_after,CleanInterval}]) ->
	lager:info("Sessing ghost timeout to:~p ms,clean interval to:~p ms",[Timeout,CleanInterval]),
	TRef = erlang:start_timer(CleanInterval, self(), trigger),
	{ok, #state{ghost_timeout=Timeout,clean_interval=CleanInterval,tref=TRef}}.




%% @private
handle_call(_Request, _From, State) ->
	{reply, {error, unknown_call}, State}.

%% @private
handle_cast(_Msg, State) ->
    
	{noreply, State}.

handle_info({timeout, _Ref, trigger}, State) ->
    timer:cancel(State#state.tref),
    process_ghost_sessions(State#state.ghost_timeout),
    TRef = erlang:start_timer(State#state.clean_interval, self(), trigger),
    NewState = State#state{tref = TRef},
    {noreply, NewState};





%% @private
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



process_ghost_sessions(Timeout)->
	lager:debug("Scanning for ghost sessions..."),
	Sessions=get_ghost_sessions(Timeout),
	GhostSessionData=get_ghost_session_data(),
	lager:debug("Got ghost session data:~p",[GhostSessionData]),
	[m_etp_session_data_proxy:delete_session_data(X) || X <-GhostSessionData],
	[m_etp_session_proxy:delete_session(X) || X <- Sessions],
	[m_etp_session_data_proxy:delete_session_data(X) || X <-Sessions].


get_ghost_sessions(Timeout) ->
	TimeThreshold=m_etp_utils:get_utc_timestamp()-Timeout,
	lager:debug("Cleaning out ghost sessions that do not have any activity and last_updated<~p",[TimeThreshold]),
    Sessions=do(qlc:q([X#m_etp_session.session_id || X <- mnesia:table(m_etp_sessions),
			     X#m_etp_session.updated < TimeThreshold 
			     
				])),
    Sessions.

%check so that we do not have entries in session data that are not represented
%in session store table
get_ghost_session_data()->
	Handle = do(qlc:q([X#m_etp_session_data.session_id || X <- mnesia:table(m_etp_session_data),
                                   Y <- mnesia:table(m_etp_sessions),
                                   X#m_etp_session_data.session_id /= Y#m_etp_session.session_id 
    ])),
    Handle.

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.