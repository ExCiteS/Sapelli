package uk.ac.ucl.excites.sapelli.transmission;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.storage.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.storage.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.transmission.compression.CompressorFactory;
import uk.ac.ucl.excites.sapelli.transmission.compression.CompressorFactory.CompressionMode;
import uk.ac.ucl.excites.sapelli.transmission.crypto.Hashing;
import uk.ac.ucl.excites.sapelli.transmission.sms.SMSAgent;
import uk.ac.ucl.excites.sapelli.transmission.sms.binary.BinarySMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

public abstract class BinaryTransmission extends Transmission
{

	static public final int TRANSMISSION_ID_SIZE = 16; // bits
	static public final IntegerRangeMapping TRANSMISSION_ID_FIELD = IntegerRangeMapping.ForSize(0, TRANSMISSION_ID_SIZE); // unsigned(!) 16 bit integer
	
	static public final int COMPRESSION_FLAG_SIZE = 2; // bits
	static public final IntegerRangeMapping COMPRESSION_FLAG_FIELD = IntegerRangeMapping.ForSize(0, COMPRESSION_FLAG_SIZE); // TODO use enum values? 
	
	protected Integer id = null; // Transmission ID: computed as a CRC16 hash over the transmission payload (unsigned 16 bit int)
	protected float compressionRatio = 1.0f;
	
	public BinaryTransmission(TransmissionClient modelProvider)
	{
		super(modelProvider);
	}
	
	public BinaryTransmission(Schema schema, Set<Column<?>> columnsToFactorOut, Settings settings)
	{
		super(schema, columnsToFactorOut, settings);
	}

	protected void preparePayload() throws IOException, TransmissionCapacityExceededException
	{
		BitOutputStream out = null;
		try
		{
			// Output stream:
			ByteArrayOutputStream rawOut = new ByteArrayOutputStream();
			out = new BitOutputStream(rawOut);
			
			// Encode records per schema
			for(Schema schema : recordsBySchema.keySet())
				encodeRecords(schema, out);
			byte[] recordBytes = rawOut.toByteArray();
			
			//System.out.println("Encoded records to: " + data.length + " bytes");
			//System.out.println("Hash: " + BinaryHelpers.toHexadecimealString(Hashing.getSHA256Hash(data)));
			
			// Compress records
			recordBytes = compress(recordBytes); // TODO make dynamic
			
			// Encrypt records
			recordBytes = encrypt(recordBytes); // TODO make dynamic
			
			// Reset streams:
			rawOut = new ByteArrayOutputStream();
			out = new BitOutputStream(rawOut);
			
			// Write complete payload:
			//	Header:
			// 		Write Model ID (32 bits):
			Schema.MODEL_ID_FIELD.write(modelID, out);
			//		Encryption flag (1 bit):
			out.write(false); // Encryption flag //TODO make dynamic
			//		Compression flag (2 bits):
			COMPRESSION_FLAG_FIELD.write(0, out); //TODO make dynamic
			//		Multi-Schema flag (1 bit):
			out.write(isMultiSchema());
			// 		Write schema identification:
			writeSchemaIdentification(out); //can throw TransmissionCapacityExceededException
			
			//	Body: Write the encoded, compressed & encrypted records:
			out.write(recordBytes);
			
			// Flush & close the stream and get payload bytes:
			out.flush();
			out.close();
			byte[] payloadBytes = rawOut.toByteArray();
			
			// Compute transmission ID (= CRC16 hash over payload):
			id = Hashing.getCRC16Hash(payloadBytes);
	
			// Serialise
			serialise(payloadBytes); //can throw TransmissionCapacityExceededException
		}
		catch(IOException e)
		{
			throw new IOException("Error on preparing payload.", e);
		}
		finally
		{
			try
			{
				if(out != null)
					out.close();
			}
			catch(Exception ignore) {}
		}
	}

	private void writeSchemaIdentification(BitOutputStream out) throws IOException, TransmissionCapacityExceededException
	{
		if(isMultiSchema())
		{
			IntegerRangeMapping numberOfDifferentSchemataInTransmissionField = getNumberOfDifferentSchemataInTransmissionField();
			int numberOfDifferentSchemata = getSchemata().size();
			
			// Compute number of bits left for the numberOfRecordPerSchemaFields (except last one) and the actual records:
			int bitsAvailableForFieldsAndRecords =	(getMaxPayloadBytes() * Byte.SIZE)										// Payload bits available
													- out.getNumberOfBitsWritten()											// Bits already used for the header
													- numberOfDifferentSchemataInTransmissionField.getSize()				// will be written below
													- (numberOfDifferentSchemata * Schema.MODEL_SCHEMA_NO_FIELD.getSize());	// will be written below
			
			// Find best schema order, resulting in the smallest amount of bits need for the numberOfRecordsPerSchemaFields (the last of which we don't need to write):
			List<Schema> bestSchemataOrder = null;
			List<IntegerRangeMapping> numberOfRecordsPerSchemaFields = null;
			int bestSumOfFieldSizesExceptLast = Integer.MAX_VALUE;
			// In each candidate order another schema is put in the last position:
			for(Schema schemaToPutLast : getSchemata())
			{
				List<Schema> schemataOrder = new ArrayList<Schema>(numberOfDifferentSchemata);
				for(Schema schema : getSchemata())
					if(schema != schemaToPutLast)
						schemataOrder.add(schema);
				schemataOrder.add(schemaToPutLast);
				
				// Get the fields for this order:
				IntegerRangeMapping[] fieldsForOrder = getNumberOfRecordsPerSchemaFields(schemataOrder, bitsAvailableForFieldsAndRecords);
				
				// Compute sum of field sizes (except last):
				int sumOfFieldSizesExceptLast = 0;
				for(int f = 0; f < fieldsForOrder.length - 1; f++)
					sumOfFieldSizesExceptLast += fieldsForOrder[f].getSize();
				
				// Better?
				if(sumOfFieldSizesExceptLast < bestSumOfFieldSizesExceptLast)
				{
					bestSchemataOrder = schemataOrder;
					bestSumOfFieldSizesExceptLast = sumOfFieldSizesExceptLast;
					numberOfRecordsPerSchemaFields = Arrays.asList(fieldsForOrder);
				}	
			}
			
			// Write ...
			// 	the number of different schemata:
			numberOfDifferentSchemataInTransmissionField.write(numberOfDifferentSchemata, out);
			//	the model schema numbers:
			for(Schema schema : bestSchemataOrder)
				Schema.MODEL_SCHEMA_NO_FIELD.write(schema.getModelSchemaNumber(), out);			
			//	the number of records per schema, except the last one:
			Iterator<IntegerRangeMapping> fieldIter = numberOfRecordsPerSchemaFields.iterator();
			for(int s = 0; s < bestSchemataOrder.size() - 1; s++) // -1 to skip last schema!
			{
				Schema schema = bestSchemataOrder.get(s);
				IntegerRangeMapping field = fieldIter.next();
				int numberOfRecords = recordsBySchema.get(schema).size();
				if(field.fits(numberOfRecords))
					field.write(numberOfRecords, out);
				else
					throw new TransmissionCapacityExceededException("Cannot fit " + numberOfRecords + " of schema " + schema.getID() + " (max allowed: " + field.getHighBound(false) + ").");	
			}
		}
		else
			// Write the model schema number:
			Schema.MODEL_SCHEMA_NO_FIELD.write(getSchemata().iterator().next().getModelSchemaNumber(), out);
	}
	
	private List<SchemaAndNumberOfRecords> readSchemaIdentification(BitInputStream in, boolean multiSchema) throws IOException
	{
		if(multiSchema)
		{
			List<Schema> schemataOrder = new ArrayList<Schema>();
			
			// Read ...
			// 	the number of different schemata:
			int numberOfDifferentSchemata = (int) getNumberOfDifferentSchemataInTransmissionField().read(in);
			//	the model schema numbers:
			//for(int i = 0 < numberOfDifferentSchemata; i++)
				//schemataOrder.add(/* client.getSchema(...) */ )
			
			int bitsAvailableForFieldsAndRecords = 500;	/*(getMaxPayloadBytes() * Byte.SIZE)	// Payload bits available
													- in.getNumberOfBitsRead();			// Bits already used */
													
			
			IntegerRangeMapping[] fieldsForOrder = getNumberOfRecordsPerSchemaFields(schemataOrder, bitsAvailableForFieldsAndRecords);
			
			// read number of records ...
			
			
			return null;
		}
		else
		{
			return null;	
		}
	}
	
	/**
	 * Note: SMSTransmission overrides this to insert a completeness check
	 * 
	 * @param schemaToUse - can be null
	 * @param settingsToUse - can be null
	 * @throws IllegalStateException
	 * @throws IOException
	 * @throws DecodeException
	 */
	@Override
	protected void receivePayload(Schema schemaToUse, Settings settingsToUse) throws IncompleteTransmissionException, IllegalStateException, IOException, DecodeException
	{
		BitInputStream in = null;
		try
		{
			// Deserialise payload:
			byte[] payloadBytes = deserialise();
			
			// Verify payload hash:
			if(this.id != Hashing.getCRC16Hash(payloadBytes))
				throw new IncompleteTransmissionException(this, "Payload hash mismatch");
			
			// Input stream:
			ByteArrayInputStream rawIn = new ByteArrayInputStream(payloadBytes);
			in = new BitInputStream(rawIn);
			
			// Read Schema ID:
			long schemaID = Schema.SCHEMA_ID_FIELD.read(in); // 36 bits, we are not on a byte boundary now.

			// Read flags:
			// 	Encryption flag (1 bit):
			boolean encrypted = in.readBit();
			//	Compression flag (2 bits):
			int compressionMode = (int) COMPRESSION_FLAG_FIELD.read(in);
			//	Multiform flag (1 bit):
			boolean multiform = in.readBit(); // TODO use this
			// Done reading flags, totalling 4 bits, we are now back on a byte boundary. 
			
			Schema schema = null; //TODO change (this is just to let it compile)
			
			// Look-up schema unless one was provided:
			if(schemaToUse == null)
			{
				schema = client.getSchema(schemaID);
				if(schema == null)
					throw new IllegalStateException("Cannot decode message because schema (ID: " + schemaID + ") is unknown");
			}
			else
			{
				schema = schemaToUse;
				System.out.println("Using provided schema (ID: " + schemaToUse.getID() + ") instead of the one indicated by the transmission (ID: " + schemaID + ").");
			}
			
			// Look-up settings & columns to factor out:
			settings = (settingsToUse == null ? client.getSettingsFor(schema) : settingsToUse);
			setColumnsToFactorOut(client.getFactoredOutColumnsFor(schema));
			
			// Read encrypted, compressed & encoded records:
			payloadBytes = in.readBytes(in.available());
			
			// Decrypt records
			payloadBytes = decrypt(payloadBytes); //TODO make use of encrypted flag to decrypt or not	
			
			// Decompress records
			payloadBytes = decompress(payloadBytes); // TODO use Compression flag!
			
			// Decode records
			//System.out.println("Decoding records from: " + data.length + " bytes");
			//System.out.println("Hash: " + BinaryHelpers.toHexadecimealString(Hashing.getSHA256Hash(data)));
			decodeRecords(payloadBytes);
		}
		finally
		{
			try
			{
				if(in != null)
					in.close();
			}
			catch(Exception ignore) {}
		}
	}
	
	/**
	 * modelID & client must be set!
	 * 
	 * @return
	 */
	private IntegerRangeMapping getNumberOfDifferentSchemataInTransmissionField()
	{
		return new IntegerRangeMapping(	2 /*at least to different schemata in a multi-schema transmission*/,
										client.getNumberOfSchemataInModel(modelID));
	}
	
	/**
	 * Creates a recordsPerSchemaField for each schema (with the optimal size).
	 * 
	 * It is assumed the records are not compressed, nor encrypted.
	 * 
	 * @param schemataOrder
	 * @param bitsAvailableForFieldsAndRecords
	 * @return
	 */
	private IntegerRangeMapping[] getNumberOfRecordsPerSchemaFields(List<Schema> schemataOrder, int bitsAvailableForFieldsAndRecords)
	{
		IntegerRangeMapping[] fields = new IntegerRangeMapping[schemataOrder.size()];
		
		// Iterative process:
		process : do
		{
			// Compute the number of bits left for records:
			int bitsAvailableForRecords = bitsAvailableForFieldsAndRecords;
			for(int i = 0; i < fields.length - 1; i++) // length - 1! Because do *not* reserve space for the field of the last schema in the schemataOrder
				bitsAvailableForRecords -= (fields[i] == null ? 1 /*bit*/ : fields[i].getSize()); 
			// Grow the size of the fields until a stable state is reached:
			for(int x = 0; x < fields.length; x++)
			{
				Schema schemaX = schemataOrder.get(x);
				// Compute the number of bits left for records of schemaX, under the assumption that all other schemata have only a single record in the transmission
				int bitsAvailableForRecordsOfSchemaX = bitsAvailableForRecords;
				for(Schema schemaY : schemataOrder)
					if(schemaY != schemaX)
						bitsAvailableForRecordsOfSchemaX -= schemaY.getMinimumSize(); // 1 record of each schemaY where Y != X
				// Compute how many records of schemaX this allows us to transmit and instantiate the corresponding field:
				int maxRecordsOfSchemaX = bitsAvailableForRecordsOfSchemaX / schemaX.getMinimumSize(); 
				IntegerRangeMapping recordsOfSchemaXField = new IntegerRangeMapping(1, maxRecordsOfSchemaX);
				// If this is the first field for schemaX or it is larger than the previous one: make this the new field for schemaX 
				if(fields[x] == null || fields[x].getSize() < recordsOfSchemaXField.getSize())
				{
					fields[x] = recordsOfSchemaXField;
					continue process; // not yet in a stable state
				}
			}
			break process; // stable state reached!
		}
		while(true);

		// Return the fields:
		return fields;
	}

	protected void encodeRecords(Schema schema, BitOutputStream out) throws IOException
	{
		try
		{
			// Write Model Schema Number:
			Schema.MODEL_SCHEMA_NO_FIELD.write(schema.getModelSchemaNumber(), out);
			
			// Write number of records (unless this is a single schema transmission):
			// field size?
			
			// Get factored out values ...
		
//			if(columnsToFactorOut != null)
//			{
//				if(records.isEmpty())
//				{
//					factoredOutValues = new HashMap<Column<?>, Object>();
//					//Store "factored out" values:
//					for(Column<?> c : columnsToFactorOut)
//						factoredOutValues.put(c, c.retrieveValue(record));
//				}
//				else
//				{	//Check if factored out values are the same
//					for(Column<?> c : columnsToFactorOut)
//					{
//						Object rValue = c.retrieveValue(record);
//						if(factoredOutValues.get(c) == null)
//						{
//							if(rValue != null)
//								throw new IllegalArgumentException("Non-matching factored out value in " + c.toString());
//						}
//						else
//						{
//							if(rValue == null || !rValue.equals(factoredOutValues.get(c)))
//								throw new IllegalArgumentException("Non-matching factored out value in " + c.toString());
//						}
//					}
//				}
//			}
			
			//Write factored out values:
			if(columnsToFactorOut != null)
				for(Column<?> c : columnsToFactorOut)
					c.writeObject(factoredOutValues.get(c), out);
			
			//Write records:
			for(Record r : recordsBySchema.get(schema))
				r.writeToBitStream(out, false /* do not include virtual columns */, columnsToFactorOut);
		}
		catch(Exception e)
		{
			throw new IOException("Error on encoding records.", e);
		}
	}

	protected void decodeRecords(byte[] data) throws DecodeException
	{
		Schema schema = null; //TODO change (this is just to let it compile)
		
		BitInputStream in = null;
		Record record = null;
		try
		{
			//Input stream:
			ByteArrayInputStream rawIn = new ByteArrayInputStream(data);
			in = new BitInputStream(rawIn);
			
			//Read factored out values:
			if(columnsToFactorOut != null)
			{
				factoredOutValues = new HashMap<Column<?>, Object>();
				for(Column<?> c : columnsToFactorOut)
					factoredOutValues.put(c, c.readValue(in));
			}
			
			//Read records:
			while(in.bitsAvailable() >= schema.getMinimumSize(false /* do not include virtual columns */, columnsToFactorOut))
			{
				record = schema.createRecord();
				record.readFromBitStream(in, false /* do not include virtual columns */, columnsToFactorOut);
				// Set factored out values:
				if(columnsToFactorOut != null)
					for(Column<?> c : columnsToFactorOut)
						c.storeObject(record, factoredOutValues.get(c));
				
				// Add the record:
				List<Record> recordsOfSchema = recordsBySchema.get(schema);
				if(recordsOfSchema == null)
				{
					recordsOfSchema = new ArrayList<Record>();
					recordsBySchema.put(schema, recordsOfSchema);
				}
				recordsOfSchema.add(record);
			}
		}
		catch(Exception e)
		{
			DecodeException de = new DecodeException("Error on decoding records.", e, schema, getRecords()); //pass schema used for decoding and records decoded so far 
			de.addRecord(record); //add last (partially decoded) record (will be ignored if null)
			recordsBySchema.clear(); //remove partially decoded records
			throw de;
		}
		finally
		{
			try
			{
				if(in != null)
					in.close();
			}
			catch(Exception ignore) {}
		}
	}

	protected byte[] compress(byte[] data) throws IOException
	{
		byte[] compressedData = CompressorFactory.getCompressor(settings.getCompressionMode()).compress(data);
		compressionRatio = ((float) compressedData.length) / data.length;
		return compressedData;
	}

	protected byte[] decompress(byte[] compressedData) throws IOException
	{
		byte[] decompressedData = CompressorFactory.getCompressor(settings.getCompressionMode()).decompress(compressedData);
		compressionRatio = ((float) compressedData.length) / decompressedData.length;
		return decompressedData;
	}

	protected byte[] encrypt(byte[] data) throws IOException
	{
		//TODO encryption	
		return data;
	}

	protected byte[] decrypt(byte[] data) throws IOException
	{
		//TODO decryption
		return data;
	}
	
	protected abstract void serialise(byte[] data) throws TransmissionCapacityExceededException, IOException;
	
	protected abstract byte[] deserialise() throws IOException;

	public int getID()
	{	
		if(id == null)
			throw new NullPointerException("Transmission ID has not been set.");
		return id.intValue();
	}
	
	public abstract int getMaxPayloadBytes();
	
	public float getCompressionRatio()
	{
		return compressionRatio;
	}
	
	private class SchemaAndNumberOfRecords
	{
		
		Schema schema;
		int numberOfRecords;

		private SchemaAndNumberOfRecords(Schema schema, int numberOfRecords)
		{
			this.schema = schema;
			this.numberOfRecords = numberOfRecords;
		}

	}
	
}
