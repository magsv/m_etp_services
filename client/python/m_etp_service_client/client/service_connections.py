from websocket import create_connection
import logging

def connect_ping_socket(host):
	logging.debug("Conneting to host:"+host)
	return create_connection("ws://"+host+"/m_etp_socket_ping")

def socket_send_and_recieve(ws,data):
	ws.send(data)
	return ws.recv()