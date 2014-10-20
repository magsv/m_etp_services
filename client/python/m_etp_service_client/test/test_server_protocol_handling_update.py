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


    def testUpdateProtocol(self):
        try:
        	requestSessionSchema=tUtils.get_requestsession_protocol()
        	logging.debug("Reading request session schema from:"+requestSessionSchema)
        	mEtp.update_protocol("http://"+tUtils.get_servername(),requestSessionSchema)
        	#mEtp.get_protocol("http://"+tUtils.get_servername(),"TEST")
        	pass
        except Exception, e:
        	raise e


if __name__ == "__main__":
    logging.basicConfig( stream=sys.stderr,level=logging.DEBUG )
    unittest.main()