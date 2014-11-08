-module (m_etp_session_time_server).
-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


-record(state,{session_timeout,clean_interval}).


start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

%% @private
init([{session_timeout,Timeout},{session_clean_after,CleanInterval}]) ->
	lager:info("Sessing session timeout to:~p ms,clean interval to:~p ms",[Timeout,CleanInterval]),
	 timer:send_after(CleanInterval,self(),{cleanout,Interval}),
	{ok, #state{session_timeout=Timeout,clean_interval=CleanInterval}}.

%% @private
handle_call(_Request, _From, State) ->
	{reply, {error, unknown_call}, State}.

%% @private
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({cleanout,Interval},State)->
	lager:info("Cleaning out possible sessions that have been left or timed out:interval ~p",[State#state.clean_interval]),
	timer:send_after(State#state.clean_interval,self(),{cleanout}),
	{noreply,State};

%% @private
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.