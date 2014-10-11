import avro.schema
from avro.datafile import DataFileReader, DataFileWriter
from avro.io import DatumReader, DatumWriter
import logging



def serializeDataToFile(schemaFile,outputFile,dataToSerialize):
    logging.debug("Parsing in avro schema:"+schemaFile)
    schema=parse_schema(schemaFile)
    logging.debug("Writing avro data to:"+outputFile)
    writer = DataFileWriter(open(outputFile, "w"), DatumWriter(), schema)
    writer.append(dataToSerialize)
    writer.close()

def parse_schema(schemaPath):
	schema = avro.schema.parse(open(schemaPath).read())
	return schema

def deserializeDataFromFile2Str(inputFile):
	logging.debug("Deserializing file:"+inputFile)
	reader = DataFileReader(open(inputFile, "r"), DatumReader())
	data=""
	for item in reader:
		data=data+str(item)
	reader.close()
	return data