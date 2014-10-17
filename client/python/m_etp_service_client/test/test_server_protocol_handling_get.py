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


    def testGetValidSchema(self):
        try:
        	schemaName="RequestSession"
        	result=mEtp.get_protocol("http://"+tUtils.get_servername(),schemaName)
        	if result!=None:
        		logging.debug("Got result:"+str(result))
        		pass
        	else:
        		self.fail("Failed in getting schema:")
        except Exception, e:
        	self.fail("Failed in getting schema:"+str(e))

    def testGetInValidSchema(self):
        try:
            schemaName="NOT_EXISTING"
            result=mEtp.get_protocol("http://"+tUtils.get_servername(),schemaName)
            if result==None:

                pass
            else:
                self.fail("Failed in getting schema:")
        except Exception, e:
            self.fail("Failed in getting schema:"+str(e))


if __name__ == "__main__":
    logging.basicConfig( stream=sys.stderr,level=logging.DEBUG )
    unittest.main()