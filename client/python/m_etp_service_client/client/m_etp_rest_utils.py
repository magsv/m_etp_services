import requests
import logging

def post_json_data_to_service(serviceUrl,data):
	headers = {'content-type': 'application/json'}
	r=requests.post(serviceUrl,data,headers=headers)
	statusCode=r.status_code
	logging.debug("Got status from server:"+str(statusCode))
	if statusCode==201:
		logging.debug("Got ok created response")
	else:
		msg="Got error from service status code:"+str(statusCode)+","+r.text
		logging.error(msg)
		raise Exception(msg)

def get_json_data_from_service(serviceUrl):
	headers = {'Accept': 'application/json'}
	r=requests.get(serviceUrl,headers=headers)
	statusCode=r.status_code
	logging.debug("Got status from server:"+str(statusCode))
	if statusCode==200:
		
		data=r.content
		return data 
	elif statusCode==404:
		logging.debug("Request not found, server responded 404")
		return None
	else:
		raise Exception("Server returned an unknown response:"+str(statusCode))

def put_json_data_to_service(serviceUrl,data):
	headers = {'content-type': 'application/json'}
	r=requests.put(serviceUrl,data,headers=headers)
	statusCode=r.status_code
	logging.debug("Got status from server:"+str(statusCode))
	if statusCode==201:
		logging.debug("Got ok put response")
	else:
		msg="Got error from service status code:"+str(statusCode)+","+r.text
		logging.error(msg)
		raise Exception(msg)

def delete_json_resource(serviceUrl):
	headers = {'Accept': 'application/json'}
	r=requests.delete(serviceUrl,headers=headers)
	statusCode=r.status_code
	logging.debug("Got status from server:"+str(statusCode))
	if statusCode==200:
		logging.debug("Resource deleted successfully")
	elif statusCode==404:
		logging.debug("Resource to delete not found")
		raise Exception("Resource to delete not found")
	else:
		raise Exception("Failed in delete of resource")


