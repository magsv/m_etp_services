import unittest
import m_etp_service_client.client.service_connections as sConn
import logging
import sys
class Test(unittest.TestCase):


    def setUp(self):
        pass


    def tearDown(self):
        pass


    def testPingETPSocketService(self):
        ws=sConn.connect_ping_socket("localhost:8080")
        result=sConn.socket_send_and_recieve(ws,"PING")
        logging.info("Recieved message:"+result)
        ws.close()
        pass


if __name__ == "__main__":
	logging.basicConfig( stream=sys.stderr,level=logging.DEBUG )
	unittest.main()