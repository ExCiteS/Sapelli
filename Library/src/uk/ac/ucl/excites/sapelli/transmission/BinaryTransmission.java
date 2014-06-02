package uk.ac.ucl.excites.sapelli.transmission;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
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
			// Encode records:
			byte[] recordBytes = encodeRecords(); //can throw TransmissionCapacityExceededException
			
			//System.out.println("Encoded records to: " + data.length + " bytes");
			//System.out.println("Hash: " + BinaryHelpers.toHexadecimealString(Hashing.getSHA256Hash(data)));
			
			// Compress records
			recordBytes = compress(recordBytes); // TODO make dynamic
			
			// Encrypt records
			recordBytes = encrypt(recordBytes); // TODO make dynamic
			
			// Output stream:
			ByteArrayOutputStream rawOut = new ByteArrayOutputStream();
			out = new BitOutputStream(rawOut);
							
			// Write complete payload:
			//	Header:
			// 		Write Model ID (32 bits):
			Schema.MODEL_ID_FIELD.write(modelID, out);
			//		Encryption flag (1 bit):
			out.write(false); // Encryption flag //TODO make dynamic
			//		Compression flag (2 bits):
			COMPRESSION_FLAG_FIELD.write(0, out); //TODO make dynamic 
			
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
	 * @return
	 * @throws IOException
	 * @throws TransmissionCapacityExceededException
	 */
	protected byte[] encodeRecords() throws IOException, TransmissionCapacityExceededException
	{
		BitOutputStream out = null;
		try
		{			
			// Get schema identification fields:
			IntegerRangeMapping numberOfDifferentSchemataInTransmissionField = getNumberOfDifferentSchemataInTransmissionField();
			IntegerRangeMapping modelSchemaNumberField = getModelSchemaNumberField();
			
			//	Find best schema order, resulting in the smallest amount of bits need for the numberOfRecordsPerSchemaFields (the last of which we don't need to write):
			List<Schema> schemataOrder = null;
			IntegerRangeMapping[] numberOfRecordsPerSchemaFields = null;
			int smallestSizeSum = Integer.MAX_VALUE;
			// In each candidate order another schema is put in the last position:
			for(Schema schemaToPutLast : getSchemata())
			{
				// Populate order:
				List<Schema> order = new ArrayList<Schema>(getSchemata().size());
				for(Schema schema : getSchemata())
					if(schema != schemaToPutLast)
						order.add(schema);
				order.add(schemaToPutLast);
				// Get the numberOfRecordsPerSchemaFields for this order:
				IntegerRangeMapping[] fieldsForOrder = getNumberOfRecordsPerSchemaFields(numberOfDifferentSchemataInTransmissionField, modelSchemaNumberField, order);
				// Compute sum of field sizes (except last):
				int sumOfFieldSizesExceptLast = 0;
				for(int f = 0; f < fieldsForOrder.length - 1; f++)
					sumOfFieldSizesExceptLast += fieldsForOrder[f].getSize();
				// Better?
				if(sumOfFieldSizesExceptLast < smallestSizeSum)
				{
					schemataOrder = order;
					numberOfRecordsPerSchemaFields = fieldsForOrder;
					smallestSizeSum = sumOfFieldSizesExceptLast;
				}
			}
			
			// Output stream:
			ByteArrayOutputStream rawOut = new ByteArrayOutputStream();
			out = new BitOutputStream(rawOut);
			
			// Write schema identification:
			// 	write the number of different schemata (unless there is only 1 schema in the model):
			if(numberOfDifferentSchemataInTransmissionField != null)
				numberOfDifferentSchemataInTransmissionField.write(getSchemata().size(), out);
			//	write the modelSchemaNumbers of the schemata occurring in the transmission (unless there is only 1 schema in the model):
			if(modelSchemaNumberField != null)
				for(Schema schema : schemataOrder)
					modelSchemaNumberField.write(schema.getModelSchemaNumber(), out);
			//	check & write the number of records per schema (except the last one is not written):
			int f = 0;
			for(Schema schema : schemataOrder)
			{
				IntegerRangeMapping field = numberOfRecordsPerSchemaFields[f++];
				int numberOfRecords = recordsBySchema.get(schema).size();
				if(field.fits(numberOfRecords))
				{
					if(f < numberOfRecordsPerSchemaFields.length - 1)
						field.write(numberOfRecords, out); // write number of records, but not for last schema
				}
				else
					throw new TransmissionCapacityExceededException("Cannot fit " + numberOfRecords + " of schema " + schema.getID() + " (max allowed: " + field.getHighBound(false) + ").");	
			}

			// Write record data per schema:
			for(Schema schema : schemataOrder)
			{
			
				// Get factored out values ...
//				if(columnsToFactorOut != null)
//				{
//					if(records.isEmpty())
//					{
//						factoredOutValues = new HashMap<Column<?>, Object>();
//						//Store "factored out" values:
//						for(Column<?> c : columnsToFactorOut)
//							factoredOutValues.put(c, c.retrieveValue(record));
//					}
//					else
//					{	//Check if factored out values are the same
//						for(Column<?> c : columnsToFactorOut)
//						{
//							Object rValue = c.retrieveValue(record);
//							if(factoredOutValues.get(c) == null)
//							{
//								if(rValue != null)
//									throw new IllegalArgumentException("Non-matching factored out value in " + c.toString());
//							}
//							else
//							{
//								if(rValue == null || !rValue.equals(factoredOutValues.get(c)))
//									throw new IllegalArgumentException("Non-matching factored out value in " + c.toString());
//							}
//						}
//					}
//				}
				//Write factored out values:
				//if(columnsToFactorOut != null)
				//	for(Column<?> c : columnsToFactorOut)
				//		c.writeObject(factoredOutValues.get(c), out);
				
				// Write records:
				for(Record r : recordsBySchema.get(schema))
					r.writeToBitStream(out, false /* do not include virtual columns */, Collections.<Column<?>> emptySet() /* (not factoring-out for now) */);
			}

			
			//Flush & close the stream and get bytes:
			out.flush();
			out.close();
			return rawOut.toByteArray();
		}
		catch(Exception e)
		{
			throw new IOException("Error on encoding records.", e);
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
	
	protected void decodeRecords(byte[] data) throws DecodeException
	{
		Schema schema = null; //TODO change (this is just to let it compile)
		
		BitInputStream in = null;
		Record record = null;
		try
		{
			List<Schema> schemata = new ArrayList<Schema>();
			
			
			//Input stream:
			ByteArrayInputStream rawIn = new ByteArrayInputStream(data);
			in = new BitInputStream(rawIn);
			
			
						
			// Read schema identification...
			// 	the number of different schemata:
			IntegerRangeMapping numberOfDifferentSchemataInTransmissionField = getNumberOfDifferentSchemataInTransmissionField();
			int numberOfDifferentSchemata = numberOfDifferentSchemataInTransmissionField != null ?
												(int) numberOfDifferentSchemataInTransmissionField.read(in) :
												1;
			//	the model schema numbers:
			IntegerRangeMapping modelSchemaNumberField = getModelSchemaNumberField();
			if(modelSchemaNumberField != null)
				for(int i = 0; i < numberOfDifferentSchemata; i++)
					schemata.add(client.getSchema((short) modelSchemaNumberField.read(in))); //TODO change client method (use modelid!)
			else
				schemata.add(client.getSchema((short) 0)); //TODO change client method (use modelid!)
			
			//	the number of records per schema:
			IntegerRangeMapping[] numberOfRecordsPerSchemaFields = getNumberOfRecordsPerSchemaFields(numberOfDifferentSchemataInTransmissionField, modelSchemaNumberField, schemata);			
			// TODO read them...	
			
			
//			//Read factored out values:
//			if(columnsToFactorOut != null)
//			{
//				factoredOutValues = new HashMap<Column<?>, Object>();
//				for(Column<?> c : columnsToFactorOut)
//					factoredOutValues.put(c, c.readValue(in));
//			}
//			
//			//Read records:
//			while(in.bitsAvailable() >= schema.getMinimumSize(false /* do not include virtual columns */, columnsToFactorOut))
//			{
//				record = schema.createRecord();
//				record.readFromBitStream(in, false /* do not include virtual columns */, columnsToFactorOut);
//				// Set factored out values:
//				if(columnsToFactorOut != null)
//					for(Column<?> c : columnsToFactorOut)
//						c.storeObject(record, factoredOutValues.get(c));
//				
//				// Add the record:
//				List<Record> recordsOfSchema = recordsBySchema.get(schema);
//				if(recordsOfSchema == null)
//				{
//					recordsOfSchema = new ArrayList<Record>();
//					recordsBySchema.put(schema, recordsOfSchema);
//				}
//				recordsOfSchema.add(record);
//			}
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
	
	/**
	 * 
	 * modelID & client must be set!
	 * 
	 * @return the field (an IntegerRangMapping object) or null in case there is only 1 schema in the model
	 * @throws IllegalStateException
	 */
	private IntegerRangeMapping getNumberOfDifferentSchemataInTransmissionField() throws IllegalStateException
	{
		int numberOfSchemataInModel = client.getNumberOfSchemataInModel(modelID);
		if(numberOfSchemataInModel > 1)
			return new IntegerRangeMapping(1, client.getNumberOfSchemataInModel(modelID));
		else if(numberOfSchemataInModel == 1)
			return null;
		else
			throw new IllegalStateException("Number of schemata in model cannot be < 1!");
	}
	
	/**
	 * 
	 * modelID & client must be set!
	 * 
	 * @return
	 * @throws IllegalStateException
	 */
	private IntegerRangeMapping getModelSchemaNumberField() throws IllegalStateException
	{
		int numberOfSchemataInModel = client.getNumberOfSchemataInModel(modelID);
		if(numberOfSchemataInModel > 1)
			return new IntegerRangeMapping(0, numberOfSchemataInModel - 1);
		else if(numberOfSchemataInModel == 1)
			return null;
		else
			throw new IllegalStateException("Number of schemata in model cannot be < 1!");
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
	private IntegerRangeMapping[] getNumberOfRecordsPerSchemaFields(IntegerRangeMapping numberOfDifferentSchemataInTransmissionField, IntegerRangeMapping modelSchemaNumberField, List<Schema> schemataOrder)
	{
		int headerSizeBits = 8 * 8; // TODO !!!!!
		int numberOfDifferentSchemata = schemataOrder.size();
		//	Compute number of bits left for the numberOfRecordPerSchemaFields (except last one) and the actual records:
		int bitsAvailableForFieldsAndRecords =	(getMaxPayloadBytes() * Byte.SIZE)							// Payload bits available
												- headerSizeBits											// Bits already used for the header
												- (numberOfDifferentSchemataInTransmissionField == null ? 0 : numberOfDifferentSchemataInTransmissionField.getSize()) // will be written below
												- (modelSchemaNumberField == null ? 0 : numberOfDifferentSchemata * modelSchemaNumberField.getSize());	// will be written below
		IntegerRangeMapping[] fields = new IntegerRangeMapping[schemataOrder.size()];
		
		// Iterative process:
		process : do
		{
			// Compute the number of bits left for records:
			int bitsAvailableForRecords = bitsAvailableForFieldsAndRecords;
			for(int i = 0; i < fields.length - 1; i++) // length - 1! Because we do *not* reserve space for the field of the last schema in the schemataOrder
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
