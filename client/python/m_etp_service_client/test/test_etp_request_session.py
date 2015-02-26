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


    def testEstablishRequestSessionWithFaultyReqSessionData(self):
        try:
            logging.debug("Testing failure of send binary and not correct request session")
           
            dataToSend=bytearray(b"Hello World")
            ws=sConn.connect_service_socket(tUtils.get_servername(),"binary")
            ws.send_binary(dataToSend)
            ws.recv() 
           
           
            
            pass
        except Exception, e:
            self.fail("Error:"+str(e))

    def testEstablishRequestSessionBinary(self):
        try:
            logging.debug("Serializing request data to avro binary without schema")
            tUtils.serializeRequestSessionToBinaryFile()
            dataToSend=fUtils.readFileToString(tUtils.get_test_storage()+"/"+tUtils.getRequestSessionAvroFileNameBinary())
            ws=sConn.connect_service_socket(tUtils.get_servername(),"binary")
            ws.send_binary(dataToSend)
            
            result=ws.recv() 
           
           
            decoded=tUtils.getHeaderAndSessionFromBinary(result)
           
            header=decoded['header']
            data=decoded['data']
            logging.debug("Got header:"+str(header))
            logging.debug("Got data:"+str(data))
            logging.debug("Sending close session")
            tUtils.serializeCloseSessionToBinaryFile()
            dataToSend=fUtils.readFileToString(tUtils.get_test_storage()+"/"+tUtils.getCloseSessionAvroFileNameBinary())
            ws.send_binary(dataToSend)
            result=ws.recv() 
            ws.close()
            
            pass
        except Exception, e:
            self.fail("Error:"+str(e))
       


if __name__ == "__main__":
    logging.basicConfig( stream=sys.stderr,level=logging.DEBUG )
    unittest.main()