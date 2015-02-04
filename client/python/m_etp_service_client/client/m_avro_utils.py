import avro.schema
from avro.datafile import DataFileReader, DataFileWriter
from avro.io import DatumReader, DatumWriter
import logging
import avro.io
import io

def serializeDataToOCFFile(schemaFile,outputFile,dataToSerialize):
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


def deserializeBinaryFromFile(schemaFile,inputFile):
	bytes_reader = io.BytesIO(open(inputFile, "rb").read() )
	decoder = avro.io.BinaryDecoder(bytes_reader)
	schema=parse_schema(schemaFile)
	reader = avro.io.DatumReader(schema)
	data = reader.read(decoder)
	return data

def deserializeBinaryFromStream(schemaFile,binaryData):
	bytes_reader = io.BytesIO(binaryData)
	decoder = avro.io.BinaryDecoder(bytes_reader)
	schema=parse_schema(schemaFile)
	reader = avro.io.DatumReader(schema)
	data = reader.read(decoder)
	return data 

def deserializeBinaryFromStreamWithHeader(headerSchemaFile,dataSchema,binaryData):

	bytes_reader = io.BytesIO(binaryData)
	decoder = avro.io.BinaryDecoder(bytes_reader)
	headerSchema=parse_schema(headerSchemaFile)
	dataSchema=parse_schema(dataSchema)
	reader = avro.io.DatumReader(headerSchema)
	header = reader.read(decoder)
	datareader=avro.io.DatumReader(dataSchema)
	data=datareader.read(decoder)
	return {'header':header,'data':data}



def serializeDataToBinaryFile(schemaFile,outputFile,dataToSerialize):
	writer = io.BytesIO()
	encoder = avro.io.BinaryEncoder(writer)
	schema=parse_schema(schemaFile)
	datum_writer = avro.io.DatumWriter(schema)
	datum_writer.write(dataToSerialize, encoder)
	
	raw_bytes=writer.getvalue()
	newFile = open (outputFile, "wb")
	newFile.write(raw_bytes)
	newFile.close()
	logging.debug("Binary data written to:"+outputFile)


def serializeDataToBinaryFileWithHeader(headerSchemaFile,dataSchemaFile,headerToSerialize,dataToSerialize,outputFile):
	writer = io.BytesIO()
	encoder = avro.io.BinaryEncoder(writer)
	schemaData=parse_schema(dataSchemaFile)
	schemaHeader=parse_schema(headerSchemaFile)
	datum_writer_header = avro.io.DatumWriter(schemaHeader)
	datum_writer_header.write(headerToSerialize, encoder)
	datum_writer = avro.io.DatumWriter(schemaData)
	datum_writer.write(dataToSerialize, encoder)
	
	raw_bytes=writer.getvalue()
	newFile = open (outputFile, "wb")
	newFile.write(raw_bytes)
	newFile.close()
	logging.debug("Binary data written to:"+outputFile)