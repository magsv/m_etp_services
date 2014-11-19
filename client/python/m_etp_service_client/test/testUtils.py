import m_etp_service_client.client.m_avro_utils as aUtils

schemaFolder="/media/magnus/hdd_1/projects/erlang/energistics/m_etp_services/etp_schemas/m_etp_schemas_optimized/"

def get_servername():
	return "localhost:8080"

def get_requestsession_protocol():
	return schemaFolder+"RequestSession.avsc"

def get_request_session_test_data():
	return {u'applicationName': u'TESTNAME', 
	u'requestedProtocols': [{u'role': u'Producer', 
	u'protocolCapabilities': {u'TEST': {u'item': u'JALLA'}}, 
	u'protocol': 4, u'protocolVersion': {u'major': 1, 
	u'patch': 0, u'minor': 0, u'revision': 0}}]}

def get_test_storage():
	return "/media/magnus/hdd_1/projects/erlang/energistics/test_avro"


def get_request_session_test_data_dict():
	return {u'applicationName': u'TESTNAME', 
	u'requestedProtocols': [{u'role': u'Producer', 
	u'protocolCapabilities': {u'TEST': 
	{u'item': u'JALLA'}}, u'protocol': 4, 
	u'protocolVersion': {u'major': 1, 
	u'patch': 0, u'minor': 0, u'revision': 0}}]}

def getRequestSessionAvroFileName():
	return "requestSession_test.avro"

def getRequestSessionAvroFileNameBinary():
	return "requestSession_test_binary.avro"


def serializeRequestSessionToFile():
    	requestSessionSchema=get_requestsession_protocol()
    	outputFile=get_test_storage()+"/"+getRequestSessionAvroFileName()
        sessionData=get_request_session_test_data_dict()
    	aUtils.serializeDataToOCFFile(requestSessionSchema,outputFile,sessionData)

def serializeRequestSessionToBinaryFile():

    	requestSessionSchema=get_requestsession_protocol()
    	outputFile=get_test_storage()+"/"+getRequestSessionAvroFileNameBinary()
        sessionData=get_request_session_test_data_dict()
        aUtils.serializeDataToBinaryFile(requestSessionSchema, outputFile,sessionData)


def getSessionRequestDataToSend():
	serializeRequestSessionToFile()
	


