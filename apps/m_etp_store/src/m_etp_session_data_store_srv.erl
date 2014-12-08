-module (m_etp_session_data_store_srv).


-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include_lib("../m_etp_store/include/m_etp_data.hrl").

-define(SERVER, ?MODULE).



start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

%% @private
init(_Args) ->
	lager:info("Started m_etp_session_data_store_srv..."),
	{ok, undefined}.


handle_call({create_session_data,SessionData},_From,State)->
	lager:debug("Creating session data for sessionid~p",[SessionData#m_etp_session_data.session_id]),
	NewRecord=SessionData#m_etp_session_data{created=m_etp_utils:get_utc_timestamp()},
	F = fun() ->
		mnesia:write(m_etp_session_data,NewRecord,write)
	end,
	Result=mnesia:activity(transaction, F),
	{reply,handle_mnesia_result(Result,NewRecord),State};

handle_call({delete_session_data,SessionId},_From,State)->
	{reply,ok,State};


handle_call({get_session_data,SessionId},_From,State)->
	{reply,ok,State};

%% @private
handle_call(_Request, _From, State) ->
	{reply, {error, unknown_call}, State}.

%% @private
handle_cast(_Msg, State) ->
	{noreply, State}.

%% @private
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

handle_mnesia_result(ok,Record)->
	 {ok,Record};
 
handle_mnesia_result({error,Reason},_Record)->
	 lager:error("Failed in creating session dataobject:~p",[Reason]),
	 {error,failed_in_create_session_data}.