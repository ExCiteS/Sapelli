package uk.ac.ucl.excites.sapelli.transmission;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.storage.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.storage.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.transmission.compression.CompressorFactory;
import uk.ac.ucl.excites.sapelli.transmission.crypto.Hashing;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

public abstract class BinaryTransmission extends Transmission
{

	static public final int TRANSMISSION_ID_SIZE = 16; // bits
	static public final IntegerRangeMapping TRANSMISSION_ID_FIELD = IntegerRangeMapping.ForSize(0, TRANSMISSION_ID_SIZE); // unsigned(!) 16 bit integer
	
	protected Integer id = null; // computed as a CRC16 hash over the transmission payload (unsigned 16 bit int)
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
			
			// Write Schema Usage ID (= Project hash):
			Schema.SCHEMA_USAGE_ID_FIELD.write(schema.getUsageID(), out); // 32 bits
			
			// Write flags
			out.write(false); // Encryption flag //TODO make dynamic
			// TODO write Compression flag (2 bits)
			out.write(false); // Multi-form flag //TODO make dynamic
			
			// Write Schema Usage Sub-ID (= Form index):
			Schema.SCHEMA_USAGE_SUB_ID_FIELD.write(schema.getUsageSubID(), out); // 4 bits
			
			// Encode records
			byte[] recordBytes = encodeRecords(); //can throw IOException
			//System.out.println("Encoded records to: " + data.length + " bytes");
			//System.out.println("Hash: " + BinaryHelpers.toHexadecimealString(Hashing.getSHA256Hash(data)));
			
			// Compress records
			recordBytes = compress(recordBytes);
			
			// Encrypt records
			recordBytes = encrypt(recordBytes);
			
			// Write the encoded, compressed & encrypted records:
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
	protected void readPayload(Schema schemaToUse, Settings settingsToUse) throws IncompleteTransmissionException, IllegalStateException, IOException, DecodeException
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
			
			// Read Schema Usage ID (= Project hash):
			int schemaUsageID = (int) Schema.SCHEMA_USAGE_ID_FIELD.read(in); // 32 bits

			// Read flags
			boolean encrypted = in.readBit(); // Encryption flag //TODO make use of this to decrypt or not			
			// TODO read Compression flag (2 bits)
			boolean multiform = in.readBit(); // TODO use this
			
			// Read Schema Usage Sub-ID (= Form index):
			int schemaUsageSubID = (int) Schema.SCHEMA_USAGE_SUB_ID_FIELD.read(in); // 4 bits
			
			if(schemaToUse != null)
				System.out.println("Using provided schema (Usage ID: " + schemaToUse.getUsageID() + "; Usage Sub-ID: " + schemaToUse.getUsageSubID() + ") instead of the one indicated by the transmission (Usage ID: " + schemaUsageID + "; Usage Sub-ID: " + schemaUsageSubID + ").");
			
			// Look-up schema unless one was provided:
			schema = (schemaToUse == null ? modelProvider.getSchema(schemaUsageID, schemaUsageSubID) : schemaToUse);
			if(schema == null)
				throw new IllegalStateException("Cannot decode message because schema (Usage ID: " + schemaUsageID + "; Usage Sub-ID: " + schemaUsageSubID + ") is unknown");
			
			// Look-up settings & columns to factor out:
			settings = (settingsToUse == null ? modelProvider.getSettingsFor(schema) : settingsToUse);
			setColumnsToFactorOut(modelProvider.getFactoredOutColumnsFor(schema));
			
			// Read encrypted, compressed & encoded records:
			payloadBytes = in.readBytes(in.available());
			
			// Decrypt records
			payloadBytes = decrypt(payloadBytes);
			
			// Decompress records
			payloadBytes = decompress(payloadBytes);
			
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

	protected byte[] encodeRecords() throws IOException
	{
		BitOutputStream out = null;
		try
		{
			//Output stream:
			ByteArrayOutputStream rawOut = new ByteArrayOutputStream();
			out = new BitOutputStream(rawOut);
			
			//Write factored out values:
			if(columnsToFactorOut != null)
				for(Column<?> c : columnsToFactorOut)
					c.writeObject(factoredOutValues.get(c), out);
			
			//Write records:
			for(Record r : records)
				r.writeToBitStream(out, columnsToFactorOut);
			
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
		Record r = null;
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
			while(in.bitsAvailable() >= schema.getMinimumSize(columnsToFactorOut))
			{
				r = new Record(schema);
				r.readFromBitStream(in, columnsToFactorOut);
				//Set factored out values:
				if(columnsToFactorOut != null)
					for(Column<?> c : columnsToFactorOut)
						c.storeObject(r, factoredOutValues.get(c));
				records.add(r);
			}
		}
		catch(Exception e)
		{
			DecodeException de = new DecodeException("Error on decoding records.", e, schema, records); //pass schema used for decoding and records decoded so far 
			de.addRecord(r); //add last (partially decoded) record (will be ignored if null)
			records.clear(); //remove partially decoded records
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
	
	public float getCompressionRatio()
	{
		return compressionRatio;
	}
	
}
