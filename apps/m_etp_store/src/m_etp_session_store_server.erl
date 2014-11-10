-module (m_etp_session_store_server).

-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("../m_etp_store/include/m_etp_data.hrl").


-define(SERVER, ?MODULE).


start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

%% @private
init(_Args) ->
	{ok, undefined}.


handle_call({create_session,SessionId},_From,State)->
	process_create(process_get(SessionId),SessionId,State);

handle_call({get_session,SessionId},_From,State)->
	{reply,process_get(SessionId),State};

handle_call({update_session_status,SessionId,SessionStatus},_From,State)->
	process_update(process_get(SessionId),{session_status,SessionId,SessionStatus},State);

handle_call({update_session_request,SessionId,SessionRequest},_From,State)->
	process_update(process_get(SessionId),{session_request,SessionId,SessionRequest},State);

handle_call({destroy_session,SessionId},_From,State)->
	process_delete(SessionId,State);



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

process_delete(SessionId,State)->
	F = fun() ->
            
			 mnesia:delete({m_etp_sessions,SessionId})
  	end,
  	Result=mnesia:transaction(F),
    {reply,handle_mnesia_result(Result,delete_session),State}.


process_create({ok,no_data_found},SessionId,State)->
	NewRecord=#m_etp_session{session_id=SessionId,created=m_etp_utils:get_utc_timestamp(),status=connected,updated=m_etp_utils:get_utc_timestamp()},
	F = fun() ->
		mnesia:write(m_etp_sessions,NewRecord,write)
	end,
	Result=mnesia:activity(transaction, F),
	{reply,handle_mnesia_result(Result,NewRecord),State};

process_create({ok,Data},_SessionId,State) when is_atom(Data)==false->
	{reply,{error,m_etp_session_exists},State}.

process_update({ok,no_data_found},{_,_SessionId,_SessionStatus},State)->
	{reply,{error,m_etp_session_not_exists},State};


process_update({ok,Data},{session_status,_SessionId,SessionStatus},State) ->
	NewRecord=Data#m_etp_session{updated=m_etp_utils:get_utc_timestamp(),status=SessionStatus},
	F = fun() ->
		mnesia:write(m_etp_sessions,NewRecord,write)
	end,
	Result=mnesia:activity(transaction, F),
	{reply,handle_mnesia_result(Result,NewRecord),State};

process_update({ok,Data},{session_request,_SessionId,SessionRequest},State) ->
	NewRecord=Data#m_etp_session{updated=m_etp_utils:get_utc_timestamp(),session_request=SessionRequest},
	F = fun() ->
		mnesia:write(m_etp_sessions,NewRecord,write)
	end,
	Result=mnesia:activity(transaction, F),
	{reply,handle_mnesia_result(Result,NewRecord),State}.




process_get(SessionId)->
   
	Result=mnesia:dirty_read(m_etp_sessions,SessionId),
	handle_mnesia_result(Result).


handle_mnesia_result([Row])->
	 {ok,Row};

handle_mnesia_result([])->
    {ok,no_data_found}.


handle_mnesia_result(ok,Record)->
	 {ok,Record};
 
handle_mnesia_result({error,Reason},_Record)->
	 lager:error("Failed in creating sessionobject:~p",[Reason]),
	 {error,failed_in_create_session_object};

handle_mnesia_result({atomic,ok},delete_session)->
	{ok,m_etp_session_deleted};

 handle_mnesia_result({aborted,Reason},delete_session)->
 	lager:error("Failed in delete of m_etp_protocol:~p",[Reason]),
	{error,failed_in_delete_m_etp_session}.
