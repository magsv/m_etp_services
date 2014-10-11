-record(m_etp_session,{
		session_id,
		created,
		user,
		status,
		session_request
	}).


-record(m_etp_protocols,{
	    name=erlang:error({required, name}),
		protocol_no,
		name_space,
		message_type,
		raw_schema,
		compiled_schema,
		created,
		v_major,
		v_minor,
		revision,
		patch,
		role,
		valid
	}).

-record(m_etp_valid_protocol,{
		name=erlang:error({required, name}),
		v_major,
		v_minor,
		revision,
		patch,
		role,
		protocol_spec


	}).

