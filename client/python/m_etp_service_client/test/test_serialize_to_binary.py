
import unittest
import logging
import sys
import m_etp_service_client.test.testUtils as tUtils
class Test(unittest.TestCase):


    def setUp(self):
        pass


    def tearDown(self):
        pass


    def testName(self):
    	requestSessionSchema=tUtils.get_requestsession_protocol()
        tUtils.serializeRequestSessionToBinaryFile()


if __name__ == "__main__":
    logging.basicConfig( stream=sys.stderr,level=logging.DEBUG )
    unittest.main()