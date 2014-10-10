-record(m_etp_session,{
		session_id,
		created,
		user,
		status,
		session_request
	}).


-record(m_etp_protocols,{
		protocol,
		name_space,
		name,
		message_type,
		raw_schema,
		compiled_schema,
		created,
		version,
		valid
	}).