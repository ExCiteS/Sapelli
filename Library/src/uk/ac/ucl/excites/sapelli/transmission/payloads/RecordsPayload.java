package uk.ac.ucl.excites.sapelli.transmission.payloads;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.shared.io.BitArrayOutputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitWrapInputStream;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.EncryptionSettings;
import uk.ac.ucl.excites.sapelli.transmission.Payload;
import uk.ac.ucl.excites.sapelli.transmission.compression.CompressorFactory.Compression;
import uk.ac.ucl.excites.sapelli.transmission.util.PayloadDecodeException;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

public class RecordsPayload extends Payload
{

	static protected final Compression[] COMPRESSION_MODES = { Compression.NONE, Compression.DEFLATE, Compression.LZMA };
	static protected final IntegerRangeMapping COMPRESSION_FLAG_FIELD = new IntegerRangeMapping(0, COMPRESSION_MODES.length - 1); 
	
	protected final Map<Schema,List<Record>> recordsBySchema;
	protected Model model;
	
	public RecordsPayload()
	{
		this.recordsBySchema = new HashMap<Schema, List<Record>>();
	}
	
	public int getType()
	{
		return BuiltinType.Records.ordinal();
	}
	
	/**
	 * To be called from the sending side
	 * 
	 * @param record
	 * @return
	 * @throws Exception
	 */
	public boolean addRecord(Record record) throws Exception
	{
		if(!isTansmissionSet())
			throw new IllegalStateException("No transmission set!");
		if(!record.isFilled())
			return false; // record is not fully filled (non-optional values are still null)
		Schema schema = record.getSchema();
		if(schema.isInternal())
			throw new IllegalArgumentException("Cannot directly transmit records of an internal schema.");
		if(recordsBySchema.isEmpty())
			// set model ID:
			model = schema.getModel();
		//	Check model ID:
		else if(model != schema.getModel())
			throw new IllegalArgumentException("The schemata of the records in a single Transmission must all belong to the same model.");

		// Add the record:
		List<Record> recordsOfSchema = recordsBySchema.get(schema);
		if(recordsOfSchema == null)
		{
			recordsOfSchema = new ArrayList<Record>();
			recordsBySchema.put(schema, recordsOfSchema);
		}
		recordsOfSchema.add(record);
		
		// Try serialising and check capacity:
		try
		{
			serialise();
		}
		catch(TransmissionCapacityExceededException tcee)
		{	// adding this record caused transmission capacity to be exceeded, so remove it and mark the transmission as full (unless there are no other records)
			recordsOfSchema.remove(record);
			if(recordsOfSchema.isEmpty())
				recordsBySchema.remove(schema);
			return false;
		}
		
		// Record was added and payload could be serialised (and fits within transmission bounds)
		return true;
	}
	
	/**
	 * @return records grouped by schema
	 */
	public Map<Schema,List<Record>> getRecordsBySchema()
	{
		return recordsBySchema;
	}
	
	/**
	 * @return flat list of records (sorted by Schema)
	 */
	public List<Record> getRecords()
	{
		List<Record> allRecords = new ArrayList<Record>();
		for(Entry<Schema, List<Record>> entry : recordsBySchema.entrySet())
			allRecords.addAll(entry.getValue());
		return allRecords;
	}
	
	/**
	 * @return all schemata for which the transmission contains records 
	 */
	public Set<Schema> getSchemata()
	{
		return recordsBySchema.keySet();
	}
	
	/**
	 * @return whether the transmission contains records of more than 1 schema
	 */
	public boolean isMultiSchema()
	{
		return recordsBySchema.keySet().size() > 1;
	}
	
	public int getNumberOfRecords()
	{
		int total = 0;
		for(List<Record> recordsOfSchema : recordsBySchema.values())
			total += recordsOfSchema.size();
		return total;
	}
	
	public boolean isEmpty()
	{
		return recordsBySchema.isEmpty();
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.Payload#doSerialise(uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream)
	 */
	@Override
	protected void write(BitOutputStream out) throws IllegalStateException, IOException, TransmissionCapacityExceededException, UnknownModelException
	{
		if(recordsBySchema.isEmpty())
			throw new IllegalStateException("Payload contains no records. Add at least 1 record before serialising.");
		try
		{
			EncryptionSettings encryptionSettings = transmission.getClient().getEncryptionSettingsFor(model);
			boolean encrypt = encryptionSettings.isAllowEncryption() && !encryptionSettings.getKeys().isEmpty();
			
			// Write HEADER PART 1 ------------------------
			//  Write format version:
			//TODO format version!
			//	Write schema identification & get schemataOrder:
			Schema[] schemataOrder = writeSchemaIdentification(out);
			//	Encryption flag (1 bit):
			out.write(encrypt);

			// Encode records -----------------------------
			BitArray recordBits = encodeRecords(schemataOrder);
			// Compress record bits with various compression modes:
			byte[][] comprResults = compress(recordBits, COMPRESSION_MODES);
			// Determine most space-efficient compression mode:
			int bestComprIdx = 0;
			for(int c = 1; c < COMPRESSION_MODES.length; c++)
				if(comprResults[c].length < comprResults[bestComprIdx].length)
					bestComprIdx = c;

			// Write HEADER PART 2 ------------------------
			//	Compression flag (2 bits):
			COMPRESSION_FLAG_FIELD.write(bestComprIdx, out);
			if(encrypt)
			{
				// TODO encryption related fields	
			}
			
			// Write BODY: the encoded, compressed & encrypted records
			if(COMPRESSION_MODES[bestComprIdx] != Compression.NONE || encrypt)
				out.write(/*encrypt ? encrypt(comprResults[bestComprIdx]) : TODO*/ comprResults[bestComprIdx]); // write byte array, first encrypting it as needed
			else
				recordBits.writeTo(out); // write bit array (avoid padding to byte boundary)			
		}
		catch(IOException e)
		{
			throw new IOException("Error on serialising payload.", e);
		}
	}
	
	/**
	 * Note: SMSTransmission overrides this to insert a completeness check
	 * 
	 * @param BitInputStream
	 * @throws IOException
	 * @throws RecordsPayloadDecodeException
	 * @throws UnknownModelException
	 * 
	 * @see uk.ac.ucl.excites.sapelli.transmission.Payload#read(uk.ac.ucl.excites.sapelli.shared.io.BitInputStream)
	 */
	@Override
	protected void read(BitInputStream in) throws IOException, PayloadDecodeException, UnknownModelException
	{
//			// Deserialise payload:
//			//byte[] payloadBytes // = deserialise();
//			
//			// Input stream:
//			ByteArrayInputStream rawIn = new ByteArrayInputStream(payloadBytes);
//			in = new BitWrapInputStream(rawIn);
//			
//			// Read Schema ID:
//			modelID = Schema.MODEL_ID_FIELD.read(in); // 56 bits
//
//			// Read flags:
//			// 	Encryption flag (1 bit):
//			boolean encrypted = in.readBit();
//			//	Compression flag (2 bits):
//			int compressionMode = (int) COMPRESSION_FLAG_FIELD.read(in);
//			
//			// Read encrypted, compressed & encoded records:
//			byte[] recordBytes = in.readBytes(in.available());
//			
//			// Decrypt records
//			recordBytes = decrypt(recordBytes); //TODO make use of encrypted flag to decrypt or not	
//			
//			// Decompress records
//			recordBytes = decompress(recordBytes); // TODO use Compression flag!
//			
//			// Decode records
//			decodeRecords(recordBytes);
	}
	
	/**
	 * @param out
	 * @return
	 * @throws IllegalStateException
	 * @throws UnknownModelException
	 * @throws IOException
	 * @throws TransmissionCapacityExceededException
	 */
	protected Schema[] writeSchemaIdentification(BitOutputStream out) throws IllegalStateException, UnknownModelException, IOException, TransmissionCapacityExceededException
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
		
		// Write schema identification:
		// 	Write Model ID (56 bits):
		Model.MODEL_ID_FIELD.write(model.getID(), out);
		// 	Write the number of different schemata (unless there is only 1 schema in the model):
		if(numberOfDifferentSchemataInTransmissionField != null)
			numberOfDifferentSchemataInTransmissionField.write(numberOfDifferentSchemataInTransmission, out);
		//	Write the modelSchemaNumbers of the schemata occurring in the transmission (unless there is only 1 schema in the model):
		if(modelSchemaNoField != null)
			for(Schema schema : schemataOrder)
				modelSchemaNoField.write(schema.getModelSchemaNumber(), out);
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
				throw new TransmissionCapacityExceededException("Cannot fit " + numberOfRecords + " of schema " + schema.getName() + " (max allowed: " + field.getHighBound(false) + ").");	
		}
		return schemataOrder;
	}

	/**
	 * @param out
	 * @return
	 * @throws IOException
	 * @throws TransmissionCapacityExceededException
	 */
	protected BitArray encodeRecords(Schema[] schemataOrder) throws IOException, TransmissionCapacityExceededException
	{
		BitArrayOutputStream out = null;
		try
		{
			out = new BitArrayOutputStream();
			
			// Encode records per schema...
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
			
			return out.toBitArray();
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
	
	protected void decodeRecords(byte[] data) throws RecordsPayloadDecodeException
	{		
		BitInputStream in = null;
		Record record = null;
		try
		{
			//Input stream:
			ByteArrayInputStream rawIn = new ByteArrayInputStream(data);
			in = new BitWrapInputStream(rawIn);
			
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
					schemata[i] = model.getSchema((int) modelSchemaNoField.read(in));
			else
				schemata[0] = model.getSchema(0); // there is only 1 schema in the model so its number is 0
			
			//	the number of records per schema:
			IntegerRangeMapping[] numberOfRecordsPerSchemaFields = getNumberOfRecordsPerSchemaFields(numberOfDifferentSchemataInTransmissionField, modelSchemaNoField, schemata);
			int[] numberOfRecordsPerSchema = new int[numberOfDifferentSchemata];
			for(int s = 0; s < numberOfDifferentSchemata; s++)
				// // read number of records for each each schema except for the last one (the number of records for that one is not stored in the header)
				numberOfRecordsPerSchema[s] =	s < numberOfDifferentSchemata - 1 ?
													(int) numberOfRecordsPerSchemaFields[s].read(in) :
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
			RecordsPayloadDecodeException de = new RecordsPayloadDecodeException(this, "Error on decoding records.", e); //pass schema used for decoding and records decoded so far 
			de.addPartialRecord(record); //add last (partially decoded) record (will be ignored if null)
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
	 * model must be set!
	 * 
	 * @return the field (an IntegerRangMapping object) or null in case there is only 1 schema in the model
	 * @throws IllegalStateException
	 * @throws UnknownModelException 
	 */
	private IntegerRangeMapping getNumberOfDifferentSchemataInTransmissionField() throws IllegalStateException, UnknownModelException
	{
		int numberOfSchemataInModel = model.getNumberOfSchemata();
		if(numberOfSchemataInModel > 1)
			return new IntegerRangeMapping(1, numberOfSchemataInModel);
		else if(numberOfSchemataInModel == 1)
			return null;
		else
			throw new IllegalStateException("Number of schemata in model cannot be < 1!");
	}
	
	/**
	 * 
	 * model must be set!
	 * 
	 * @return
	 * @throws IllegalStateException
	 * @throws UnknownModelException 
	 */
	private IntegerRangeMapping getModelSchemaNoField() throws IllegalStateException, UnknownModelException
	{
		int numberOfSchemataInModel = model.getNumberOfSchemata();
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
		int bitsAvailableForFieldsAndRecords =	transmission.getMaxPayloadBits()							// Payload bits available
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
				IntegerRangeMapping recordsOfSchemaXField = new IntegerRangeMapping(1, maxRecordsOfSchemaX); // at least 1 record!
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
	
}
