-module (m_etp_valid_protocol_store_srv).
-export([start_link/1]).

-include_lib("../m_etp_store/include/m_etp_data.hrl").


-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link(Args) ->
    lager:info("~p started",[?MODULE]),
	gen_server:start_link(?MODULE, Args, []).

%% @private
init({}) ->
	{ok, undefined}.


handle_call({get_valid_protocols},_From,State)->
	%%to do-----add proper handling of supported protocols
    Supported=[build_record(1,1,0,0,0,consumer),build_record(2,1,0,0,0,consumer),build_record(1,1,0,0,0,producer),build_record(1,1,0,0,0,producer)],
	{reply, {ok, Supported}, State};

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

build_record(Protocol,Major,Minor,Revision,Patch,Role)->
	#m_etp_valid_protocol{protocol_no=Protocol,v_major=Major,v_minor=Minor,revision=Revision,patch=patch,role=Role}.