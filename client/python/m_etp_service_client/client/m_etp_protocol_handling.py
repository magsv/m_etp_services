import logging
import m_etp_service_client.client.m_file_utils as fUtils
import m_etp_service_client.client.m_etp_rest_utils as rUtils



def store_protocol(serverName,protocolFile):
	try:
		protocolServiceBase=serverName+"/m_etp_protocol_service"
		logging.debug("Sending data to:"+protocolServiceBase)
		data=fUtils.readFileToString(protocolFile)
		rUtils.post_json_data_to_service(protocolServiceBase,data)
	except Exception, e:
		raise e


def get_protocol(serverName,protocolName):
	try:
		protocolServiceBase=serverName+"/m_etp_protocol_service"+"/"+protocolName
		logging.debug("Getting data from:"+protocolServiceBase)
		
		result=rUtils.get_json_data_from_service(protocolServiceBase)
		return result
	except Exception, e:
		raise Exception("Failed in get protocol:"+str(e))