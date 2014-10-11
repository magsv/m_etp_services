from websocket import create_connection
import logging

def connect_ping_socket(host,protocol="energistics-tp"):
	logging.debug("Conneting to host:"+host)
	return create_connection("ws://"+host+"/m_etp_socket_ping",
		header=["Sec-Websocket-Protocol: "+protocol])

def connect_service_socket(host):
	logging.debug("Conneting to host:"+host)
	return create_connection("ws://"+host+"/m_etp_socket_service",
		header=["Sec-Websocket-Protocol: energistics-tp"])

def socket_send_and_recieve(ws,data):
	ws.send(data)
	return ws.recv()

def socket_send_protocol_data(dataFile):
	with open (dataFile, "r") as avrofile:
		data=avrofile.read()
	logging.debug("Parsed in avrofile to:"+data)


