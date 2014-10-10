-module (m_etp_utils).

-export([get_session_token/0]).


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