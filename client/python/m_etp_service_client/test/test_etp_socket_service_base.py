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


    def testEstablishConnection(self):
    	try:
    		logging.debug("Testing establishin connection")
    		ws=sConn.connect_service_socket(tUtils.get_servername())
    		ws.close()
    		pass
    	except Exception, e:
    		self.fail("Error:"+str(e))

    def testFailConnectionWithWrongProtocol(self):
        try:
            ws=sConn.connect_service_socket(tUtils.get_servername(),"INVALID_PROTOCOL")
            ws.close()
            self.fail()
        except Exception, e:
           pass
        


if __name__ == "__main__":
	logging.basicConfig( stream=sys.stderr,level=logging.DEBUG )
	unittest.main()