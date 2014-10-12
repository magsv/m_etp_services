import logging


def readFileToString(filePath):
	logging.debug("Reading file:"+filePath)
	return open(filePath,'r').read()