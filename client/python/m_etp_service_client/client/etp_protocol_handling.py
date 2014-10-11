import avro.schema
from avro.datafile import DataFileReader, DataFileWriter
from avro.io import DatumReader, DatumWriter

def parse_schema(schemaPath):
	schema = avro.schema.parse(open(schemaPath).read())
	return schema


def serializeOpenSessionRequest(schema,sessionData,outputFile):
	writer = DataFileWriter(open(outputFile, "w"), DatumWriter(), schema)
	writer.append(sessionData)
	writer.close()
