-module (m_etp_session_store_server).

-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link(Args) ->
	lager:info("M_ETP_SESSION server started"),
	gen_server:start_link(?MODULE, Args, []).

%% @private
init({}) ->
	{ok, undefined}.

handle_call({create_session},From,State)->
	ok;

handle_call({get_session,Token},From,State)->
	ok;

handle_call({destroy_session},From,State)->
	ok;

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