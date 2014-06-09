package uk.ac.ucl.excites.sapelli.transmission;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import uk.ac.ucl.excites.sapelli.storage.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.storage.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.compression.CompressorFactory;
import uk.ac.ucl.excites.sapelli.transmission.crypto.Hashing;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

public abstract class BinaryTransmission extends Transmission
{

	static public final int TRANSMISSION_ID_SIZE = 16; // bits
	static public final IntegerRangeMapping TRANSMISSION_ID_FIELD = IntegerRangeMapping.ForSize(0, TRANSMISSION_ID_SIZE); // unsigned(!) 16 bit integer
	
	static public final int COMPRESSION_FLAG_SIZE = 2; // bits
	static public final IntegerRangeMapping COMPRESSION_FLAG_FIELD = IntegerRangeMapping.ForSize(0, COMPRESSION_FLAG_SIZE); // TODO use enum values? 
	
	protected Integer id = null; // Transmission ID: computed as a CRC16 hash over the transmission payload (unsigned 16 bit int)
	protected float compressionRatio = 1.0f;
	
	public BinaryTransmission(TransmissionClient client)
	{
		super(client);
	}
	
	public BinaryTransmission()
	{
		super();
	}

	protected void preparePayload() throws IOException, TransmissionCapacityExceededException
	{
		BitOutputStream out = null;
		try
		{
			// Encode records:
			byte[] recordBytes = encodeRecords(); // can throw TransmissionCapacityExceededException
			
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
			
//			// Look-up schema unless one was provided:
//			if(schemaToUse == null)
//			{
//				schema = client.getSchema(schemaID);
//				if(schema == null)
//					throw new IllegalStateException("Cannot decode message because schema (ID: " + schemaID + ") is unknown");
//			}
//			else
//			{
//				schema = schemaToUse;
//				System.out.println("Using provided schema (ID: " + schemaToUse.getID() + ") instead of the one indicated by the transmission (ID: " + schemaID + ").");
//			}
			
			// Look-up settings & columns to factor out:
			settings = (settingsToUse == null ? client.getSettingsFor(schema) : settingsToUse);
			
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
			int numberOfDifferentSchemataInTransmission = getSchemata().size();
			
			// Get schema identification fields:
			IntegerRangeMapping numberOfDifferentSchemataInTransmissionField = getNumberOfDifferentSchemataInTransmissionField();
			IntegerRangeMapping modelSchemaNoField = getModelSchemaNoField();
			
			//	Find best schema order, resulting in the smallest amount of bits need for the numberOfRecordsPerSchemaFields (the last of which we don't need to write):
			Schema[] schemataOrder = null;
			IntegerRangeMapping[] numberOfRecordsPerSchemaFields = null;
			int smallestSizeSum = Integer.MAX_VALUE;
			// In each candidate order another schema is put in the last position:
			for(Schema schemaToPutLast : getSchemata())
			{
				// Populate order:
				Schema[] candidateOrder = new Schema[numberOfDifferentSchemataInTransmission];
				int s = 0;
				for(Schema schema : getSchemata())
					if(schema != schemaToPutLast)
						candidateOrder[s++] = schema;
				candidateOrder[numberOfDifferentSchemataInTransmission - 1] = schemaToPutLast;
				// Get the numberOfRecordsPerSchemaFields for this order:
				IntegerRangeMapping[] fieldsForOrder = getNumberOfRecordsPerSchemaFields(numberOfDifferentSchemataInTransmissionField, modelSchemaNoField, candidateOrder);
				// Compute sum of field sizes (except last):
				int sumOfFieldSizesExceptLast = 0;
				for(int f = 0; f < fieldsForOrder.length - 1; f++)
					sumOfFieldSizesExceptLast += fieldsForOrder[f].getSize();
				// Better?
				if(sumOfFieldSizesExceptLast < smallestSizeSum)
				{
					schemataOrder = candidateOrder;
					numberOfRecordsPerSchemaFields = fieldsForOrder;
					smallestSizeSum = sumOfFieldSizesExceptLast;
				}
			}
			
			// Output stream:
			ByteArrayOutputStream rawOut = new ByteArrayOutputStream();
			out = new BitOutputStream(rawOut);
			
			// Write schema identification:
			// 	Write the number of different schemata (unless there is only 1 schema in the model):
			if(numberOfDifferentSchemataInTransmissionField != null)
				numberOfDifferentSchemataInTransmissionField.write(numberOfDifferentSchemataInTransmission, out);
			//	Write the modelSchemaNumbers of the schemata occurring in the transmission (unless there is only 1 schema in the model):
			if(modelSchemaNoField != null)
				for(Schema schema : schemataOrder)
					modelSchemaNoField.write(schema.getModelSchemaNo(), out);
			//	Check & write the number of records per schema (except the last one is not written):
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

			// Per schema...
			for(Schema schema : schemataOrder)
			{
				List<Record> records = recordsBySchema.get(schema);
				Map<Column<?>, Object> factoredOutValues = Collections.<Column<?>, Object> emptyMap();
				
				// Get factored out values ...
				if(records.size() > 1)
				{
					factoredOutValues = new LinkedHashMap<Column<?>, Object>();
					boolean first = true;
					for(Record r : records)
					{
						if(first)
						{	// get values of first record:
							for(Column<?> c : schema.getColumns(false))
								factoredOutValues.put(c, c.retrieveValue(records.get(0))); // treat all columns as potentially factored-out
							first = false;
						}
						else
						{	//Check if these values are these same in subsequent records:
							for(Column<?> factoredOutCol : factoredOutValues.keySet())
								if(!factoredOutValues.get(factoredOutCol).equals(factoredOutCol.retrieveValue(r)))
									factoredOutValues.remove(factoredOutCol); // value mismatch -> this column can not be factored out
							if(factoredOutValues.isEmpty())
								break; // no factored-out columns left -> no need to loop over rest of the records
						}
					}
				}
				
				// Write factoring-out header (& factored-out values, if used):
				if(!factoredOutValues.isEmpty())
				{
					// Write flag which indicates that factoring-out is used:
					out.write(true);
					// Write flags which indicate which columns are factored out (= 1) and which are not (= 0):
					for(Column<?> c : schema.getColumns(false))
						out.write(factoredOutValues.containsKey(c));
					// Write factored out values:
					for(Entry<Column<?>, Object> fEntry : factoredOutValues.entrySet())
						fEntry.getKey().writeObject(fEntry.getValue(), out);
				}
				else
					// Write flag which indicates that factoring-out is *not* used:
					out.write(false);
				
				// Write record data, skipping virtual columns and factored-out columns:
				for(Record r : recordsBySchema.get(schema))
					r.writeToBitStream(out, false /* do not include virtual columns */, factoredOutValues.keySet());
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
		BitInputStream in = null;
		Record record = null;
		try
		{
			//Input stream:
			ByteArrayInputStream rawIn = new ByteArrayInputStream(data);
			in = new BitInputStream(rawIn);
			
			// Read schema identification...
			// 	the number of different schemata:
			IntegerRangeMapping numberOfDifferentSchemataInTransmissionField = getNumberOfDifferentSchemataInTransmissionField();
			int numberOfDifferentSchemata = numberOfDifferentSchemataInTransmissionField != null ?
												(int) numberOfDifferentSchemataInTransmissionField.read(in) :
												1;
			Schema[] schemata = new Schema[numberOfDifferentSchemata];
			//	the model schema numbers:
			IntegerRangeMapping modelSchemaNoField = getModelSchemaNoField();
			if(modelSchemaNoField != null)
				for(int i = 0; i < numberOfDifferentSchemata; i++)
					schemata[i] = client.getSchema(modelID, (short) modelSchemaNoField.read(in));
			else
				schemata[0] = client.getSchema(modelID, (short) 0); // there is only 1 schema in the model so its number is 0
			
			//	the number of records per schema:
			IntegerRangeMapping[] numberOfRecordsPerSchemaFields = getNumberOfRecordsPerSchemaFields(numberOfDifferentSchemataInTransmissionField, modelSchemaNoField, schemata);
			int[] numberOfRecordsPerSchema = new int[numberOfDifferentSchemata];
			for(int i = 0; i < numberOfDifferentSchemata; i++)
				// // read number of records for each each schema except for the last one (the number of records for that one is not stored in the header)
				numberOfRecordsPerSchema[i] =	i < numberOfDifferentSchemata - 1 ?
													(int) numberOfRecordsPerSchemaFields[i].read(in) :
													Integer.MAX_VALUE; // no limit, reading of records will be limited by available bits (see below)

			// Per schema...
			for(int s = 0; s < schemata.length; s++)
			{
				Schema schema = schemata[s];
				Map<Column<?>, Object> factoredOutValues = Collections.<Column<?>, Object> emptyMap();
				
				// Read factoring-out header (& factored-out values, if used)...
				if(in.readBit()) //	read flag that indicates whether or not some columns are factored-out
				{
					List<Column<?>> factoredOutColumns = new ArrayList<Column<?>>();
					// Read which columns are factored out:
					for(Column<?> c : schema.getColumns(false))
						if(in.readBit())
							factoredOutColumns.add(c);
					// Read factored out values:
					factoredOutValues = new HashMap<Column<?>, Object>();
					for(Column<?> fCol : factoredOutColumns)
						factoredOutValues.put(fCol, fCol.readValue(in));
				}
				
				// Create & store list for the records that will be decoded:
				List<Record> recordsOfSchema = recordsBySchema.get(schema);
				recordsBySchema.put(schema, recordsOfSchema);
				
				// Read record data:
				while(	recordsOfSchema.size() < numberOfRecordsPerSchema[s] &&					
						in.bitsAvailable() >= schema.getMinimumSize(false /* do not include virtual columns */, factoredOutValues.keySet()))
				{
					// Get new Record instance:
					record = schema.createRecord();
					// Read record values from the stream, skipping virtual columns and factored-out columns:
					record.readFromBitStream(in, false /* do not include virtual columns */, factoredOutValues.keySet());
					// Set factored out values:
					if(!factoredOutValues.isEmpty())
						for(Entry<Column<?>, Object> fEntry : factoredOutValues.entrySet())
							fEntry.getKey().storeObject(record, fEntry.getValue());
					// Add the record:
					recordsOfSchema.add(record);				
				}
			}
		}
		catch(Exception e)
		{
			DecodeException de = new DecodeException("Error on decoding records.", e, modelID, getRecords()); //pass schema used for decoding and records decoded so far 
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
	 * @throws UnknownModelException 
	 */
	private IntegerRangeMapping getNumberOfDifferentSchemataInTransmissionField() throws IllegalStateException, UnknownModelException
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
	 * @throws UnknownModelException 
	 */
	private IntegerRangeMapping getModelSchemaNoField() throws IllegalStateException, UnknownModelException
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
	 * @param numberOfDifferentSchemataInTransmissionField
	 * @param modelSchemaNumberField
	 * @param schemataOrder
	 * @return
	 */
	private IntegerRangeMapping[] getNumberOfRecordsPerSchemaFields(IntegerRangeMapping numberOfDifferentSchemataInTransmissionField, IntegerRangeMapping modelSchemaNumberField, Schema[] schemataOrder)
	{
		int headerSizeBits = 8 * 8; // TODO !!!!!
		//	Compute number of bits left for the numberOfRecordPerSchemaFields (except last one) and the actual records:
		int bitsAvailableForFieldsAndRecords =	(getMaxPayloadBytes() * Byte.SIZE)							// Payload bits available
												- headerSizeBits											// Bits already used for the header
												- (numberOfDifferentSchemataInTransmissionField == null ? 0 : numberOfDifferentSchemataInTransmissionField.getSize()) // will be written below
												- (modelSchemaNumberField == null ? 0 : schemataOrder.length * modelSchemaNumberField.getSize());	// will be written below
		IntegerRangeMapping[] fields = new IntegerRangeMapping[schemataOrder.length];
		
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
				Schema schemaX = schemataOrder[x];
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
	
}
