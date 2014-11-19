-module (m_etp_session_time_server).
-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("../m_etp_store/include/m_etp_data.hrl").


-define(SERVER, ?MODULE).


-record(state,{session_timeout,clean_interval,tref}).


start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

%% @private
init([{session_timeout,Timeout},{session_clean_after,CleanInterval}]) ->
	lager:info("Sessing session timeout to:~p ms,clean interval to:~p ms",[Timeout,CleanInterval]),
	TRef = erlang:start_timer(CleanInterval, self(), trigger),
	{ok, #state{session_timeout=Timeout,clean_interval=CleanInterval,tref=TRef}}.




%% @private
handle_call(_Request, _From, State) ->
	{reply, {error, unknown_call}, State}.

%% @private
handle_cast(_Msg, State) ->
    
	{noreply, State}.

handle_info({timeout, _Ref, trigger}, State) ->
    timer:cancel(State#state.tref),
    process_time_out_sessions(State#state.session_timeout),
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



process_time_out_sessions(Timeout)->
	lager:info("Scanning for timed out sessions..."),
	Sessions=get_timed_out_sessions(Timeout),
	[{m_etp_session_proxy:delete_session(X)} || X <- Sessions].


get_timed_out_sessions(Timeout) ->
	TimeThreshold=m_etp_utils:get_utc_timestamp()-Timeout,
	lager:info("Cleaning out sessions that do not have state connected  and last_updated<~p",[TimeThreshold]),
    Sessions=do(qlc:q([X#m_etp_session.session_id || X <- mnesia:table(m_etp_sessions),
			     X#m_etp_session.updated < TimeThreshold 
			     ,X#m_etp_session.status/=connected
				])),
    Sessions.
    

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.