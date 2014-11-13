-module (m_etp_avro_codec_srv).
-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

%% @private
init(_Args) ->
	lager:info("Started m_etp_avro_coded srv instance..."),
	{ok, undefined}.



handle_call({decode,binary,Data},_From,State)->
	lager:info("IN decode binary"),
	try eavro_ocf_codec:decode(<<"Data">>,undefined) of 
		Any ->
			lager:info("Decoded ok")
	catch 
		_:Reason -> 
			lager:error("Failed in decode:~p",[Reason])
		
	end,
	{reply,{ok},State};


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
terminate(Reason, _State) ->
	lager:error("Avro codec server terminate with reason:~p",[Reason]),
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.