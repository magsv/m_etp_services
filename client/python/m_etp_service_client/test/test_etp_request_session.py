import unittest
import logging
import sys
import m_etp_service_client.client.m_service_connections as sConn
import m_etp_service_client.test.testUtils as tUtils
import m_etp_service_client.client.m_file_utils as fUtils

class Test(unittest.TestCase):


    def setUp(self):
        pass


    def tearDown(self):
        pass


    def testEstablishRequestSession(self):
    	try:
            logging.debug("Serializing request data to avron")
            tUtils.serializeRequestSessionToFile()
            dataToSend=fUtils.readFileToString(tUtils.get_test_storage()+"/"+tUtils.getRequestSessionAvroFileName())
            ws=sConn.connect_service_socket(tUtils.get_servername())
            #ws=sConn.connect_service_socket("localhost:8081")
            #result =  ws.recv()
            #print "Received '%s'" % result
            ws.send_binary(dataToSend)
            ws.close()
            pass
        except Exception, e:
    		self.fail("Error:"+str(e))
       


if __name__ == "__main__":
    logging.basicConfig( stream=sys.stderr,level=logging.DEBUG )
    unittest.main()