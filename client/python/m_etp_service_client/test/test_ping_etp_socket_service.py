import unittest
import m_etp_service_client.client.m_service_connections as sConn
import logging
import sys
import m_etp_service_client.test.testUtils as tUtils
class Test(unittest.TestCase):


    def setUp(self):
        pass


    def tearDown(self):
        pass


    def testPingETPSocketService(self):
        ws=sConn.connect_ping_socket(tUtils.get_servername())
        logging.debug("Connected sending data")
        result=sConn.socket_send_and_recieve(ws,"PING")
        logging.info("Recieved message:"+result)
        ws.close()
        pass


    def testFailPingETPServiceWithWrongProtocol(self):
        try:
            ws=sConn.connect_ping_socket(tUtils.get_servername(),"INVALID_PROTOCOL")
            ws.close()
            self.fail()
        except Exception, e:
           
           pass


if __name__ == "__main__":
	logging.basicConfig( stream=sys.stderr,level=logging.DEBUG )
	unittest.main()