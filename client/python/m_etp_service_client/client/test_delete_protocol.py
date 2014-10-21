import unittest
import logging
import sys
import m_etp_service_client.client.m_etp_protocol_handling as mEtp
import m_etp_service_client.test.testUtils as tUtils
class Test(unittest.TestCase):


    def setUp(self):
        pass


    def tearDown(self):
        pass


    def testDeleteProtocol(self):
        try:
        	schemaName="RequestSession"
        	
        	mEtp.delete_protocol("http://"+tUtils.get_servername(),schemaName) 
        	logging.debug("Deleted schema...")      	
        	pass
        except Exception, e:
        	self.fail("Failed in delete schema:"+str(e))


if __name__ == "__main__":
    logging.basicConfig( stream=sys.stderr,level=logging.DEBUG )
    unittest.main()