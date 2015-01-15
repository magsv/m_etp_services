-record(m_etp_session,{
		session_id,
		created,
		updated,
		user,
		status,
		session_request
	}).

-record(m_etp_session_data,{
		session_id,
		user,
		protocol,
		v_major,
		v_minor,
		revision,
		patch,
		role,
		capabilities,
		created,
		updated,
		application_name

	}).


-record(m_etp_protocol,{
		full_name=erlang:error({required,full_name}),
	    name=erlang:error({required, name}),
		protocol_no=erlang:error({required, protocol_no}),
		message_type=erlang:error({required, message_type}),
		name_space=erlang:error({required, name_space}),
		raw_schema,
		compiled_schema,
		created,
		updated,
		v_major,
		v_minor,
		revision,
		patch,
		role,
		valid
	}).

-record(m_etp_valid_protocol,{
		name=erlang:error({required, name}),
		protocol_no,
		v_major,
		v_minor,
		revision,
		patch,
		role,
		protocol_spec


	}).

