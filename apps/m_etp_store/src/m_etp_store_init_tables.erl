-module (m_etp_store_init_tables).


-include_lib("../m_etp_store/include/m_etp_data.hrl").
-export([create_tables/1,create_schema/1]).

create_tables(Nodes)->
	ensure_started(mnesia),
	ResultSession=mnesia:create_table(m_etp_sessions,[{disc_copies, Nodes }, 
							  {record_name, m_etp_session},
							  {attributes,record_info(fields,m_etp_session)}]),
	ResultSchema=mnesia:create_table(m_etp_protocol,[{disc_copies, Nodes }, 
							  {record_name, m_etp_protocol},
							  {attributes,record_info(fields,m_etp_protocol)}]),
	ResultProtocolVersions=mnesia:create_table(m_etp_valid_protocols,[{disc_copies, Nodes }, 
							  {record_name, m_etp_valid_protocol},
							  {attributes,record_info(fields,m_etp_valid_protocol)}]),
	{[ResultSession,ResultSchema,ResultProtocolVersions]}. 

create_schema(Nodes)->
	mnesia:create_schema(Nodes).

%% ====================================================================
%% Internal functions
%% ====================================================================


ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.