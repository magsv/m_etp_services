import unittest
import logging
import sys
import m_etp_service_client.test.testUtils as tUtils
import m_etp_service_client.client.avro_utils as aUtils

class Test(unittest.TestCase):
    
    

    def setUp(self):
        pass


    def tearDown(self):
        pass

    	
    def testSerializeRequestSession(self):
    	try:
    		tUtils.serializeRequestSessionToFile()
    		pass
    	except Exception, e: 
    		self.fail("Failed:"+str(e))
    	
    def testSerializeAndDeserializeRequestSessionFile(self):
    	try:
            tUtils.serializeRequestSessionToFile()
            self.deserializeRequestSessionFile()
            pass
    	except Exception, e:
    		raise e


    def deserializeRequestSessionFile(self):
        sessionAvroFile=tUtils.get_test_storage()+"/"+tUtils.getRequestSessionAvroFileName()
        result=aUtils.deserializeDataFromFile2Str(sessionAvroFile)
        logging.debug("DESERIALIZED_DATA:"+result)
        return result

    



if __name__ == "__main__":
	logging.basicConfig( stream=sys.stderr,level=logging.DEBUG )
	unittest.main()