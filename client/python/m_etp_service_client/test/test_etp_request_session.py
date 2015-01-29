import unittest
import logging
import sys
import m_etp_service_client.client.m_service_connections as sConn
import m_etp_service_client.test.testUtils as tUtils
import m_etp_service_client.client.m_file_utils as fUtils
import time
class Test(unittest.TestCase):


    def setUp(self):
        pass


    def tearDown(self):
        pass


    '''def testEstablishRequestSessionOCF(self):
    	try:
            logging.debug("Serializing request data to avro ocf")
            tUtils.serializeRequestSessionToFile()
            dataToSend=fUtils.readFileToString(tUtils.get_test_storage()+"/"+tUtils.getRequestSessionAvroFileName())
            ws=sConn.connect_service_saocket(tUtils.get_servername(),"binary_ocf")
            ws.send_binary(dataToSend)
            #time.sleep(100)
            ws.close()
            logging.debug("Closed session brutally...")
            pass
        except Exception, e:
    		self.fail("Error:"+str(e))'''

    def testEstablishRequestSessionBinary(self):
        try:
            logging.debug("Serializing request data to avro binary without schema")
            tUtils.serializeRequestSessionToBinaryFile()
            dataToSend=fUtils.readFileToString(tUtils.get_test_storage()+"/"+tUtils.getRequestSessionAvroFileNameBinary())
            ws=sConn.connect_service_socket(tUtils.get_servername(),"binary")
            ws.send_binary(dataToSend)
            result=ws.recv() 
            logging.debug("Got result:"+str(result))
            #ws.send_binary(dataToSend)
            ws.close()
            logging.debug("Closed session brutally...")
            pass
        except Exception, e:
            self.fail("Error:"+str(e))
       


if __name__ == "__main__":
    logging.basicConfig( stream=sys.stderr,level=logging.DEBUG )
    unittest.main()