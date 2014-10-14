-module (m_etp_protocol_store_server).

-include_lib("../m_etp_store/include/m_etp_data.hrl").

-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link(Args) ->
    lager:info("M_ETP_PROTOCOL_STORE_SERVER started"),
	gen_server:start_link(?MODULE, Args, []).

%% @private
init({}) ->
	{ok, undefined}.

handle_call({get,Name},_From,State)->
	{reply,process_get(Name),State};


handle_call({create,ProtocolRecord},_From,State)->
	process_create(process_get(ProtocolRecord#m_etp_protocol.name),ProtocolRecord,State);

handle_call({update,ProtocolRecord},_From,State)->
	{reply,ok,State};

handle_call({delete,ProtocolName},_From,State)->
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

process_get(ProtocolName)->
	Result=mnesia:dirty_read(m_etp_protocol,ProtocolName),
	handle_mnesia_result(Result).

process_create({ok,no_data_found},ProtocolRecord,State)->
	F = fun() ->
		mnesia:write(m_etp_protocol,ProtocolRecord,write)
	end,
	Result=mnesia:activity(transaction, F),
	{reply,handle_mnesia_result(Result,ProtocolRecord),State};
	

process_create({ok,Data},ProtocolRecord,State) when is_atom(Data)==false->
	{reply,{error,m_etp_protocol_exists},State}.


handle_mnesia_result([Row])->
	 {ok,Row};

handle_mnesia_result([])->
    {ok,no_data_found}.

handle_mnesia_result(ok,ProtocolRecord)->
	 {ok,ProtocolRecord};
 
handle_mnesia_result({error,Reason},ProtocolRecord)->
	 lager:error("Failed in creating protocolobject:~p",[Reason]),
	 {error,failed_in_create_protocol_object}.