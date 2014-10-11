-module (m_etp_utils).

-export([get_session_token/0]).
-export([get_time_and_date_list/0,get_utc_timestamp/0,get_timestamp/0]).

get_session_token()->
   create_guuid_string() ++ to_hex(crypto:rand_bytes(9)).

create_guuid_string()->
	GUUID=uuid:to_string(uuid:v4()),
	GUUID.

to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].
 
to_digit(N) when N < 10 -> $0 + N;
to_digit(N) -> $a + N-10.



get_time_and_date_list()-> 
	
	{Mega, Secs, _} = now(),
	{{Year, Month, Day}, {Hour, Minute, Second}} = erlang:localtime(),
	Timestamp = Mega*1000000 + Secs,
	[{current_utc_time,Timestamp},{year,Year},{month,Month},{day,Day},{hour,Hour},{minute,Minute},{seconds,Second}].

get_utc_timestamp()->
	CurrentTimeList=get_time_and_date_list(),
	general_list_utils:get_value_from_key(CurrentTimeList, current_utc_time).

get_timestamp() ->
{Mega,Sec,Micro} = erlang:now(),
(Mega*1000000+Sec)*1000000+Micro.