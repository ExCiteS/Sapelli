package uk.ac.ucl.excites.transmission;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Set;

import uk.ac.ucl.excites.storage.io.BitInputStream;
import uk.ac.ucl.excites.storage.io.BitOutputStream;
import uk.ac.ucl.excites.storage.model.Column;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.transmission.compression.CompressorFactory;
import uk.ac.ucl.excites.transmission.util.TransmissionCapacityExceededException;

public abstract class BinaryTransmission extends Transmission
{

	protected float compressionRatio = 1.0f;
	
	public BinaryTransmission(ModelProvider modelProvider)
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
			// Encode records
			byte[] data = encodeRecords(); //can throw IOException
			//System.out.println("Encoded records to: " + data.length + " bytes");
			//System.out.println("Hash: " + BinaryHelpers.toHexadecimealString(Hashing.getSHA256Hash(data)));
			
			// Compress records
			data = compress(data);
			
			// Encrypt records
			data = encrypt(data);
			
			// Output stream:
			ByteArrayOutputStream rawOut = new ByteArrayOutputStream();
			out = new BitOutputStream(rawOut);
			
			// Write schema ID & version:
			Schema.SCHEMA_ID_FIELD.write(schema.getID(), out);
			Schema.SCHEMA_VERSION_FIELD.write(schema.getVersion(), out);
			
			// Write the encoded, compressed & encrypted records:
			out.write(data);
			
			// Flush & close the stream and get bytes:
			out.flush();
			out.close();
			data = rawOut.toByteArray();
	
			// Serialise
			serialise(data); //can throw TransmissionCapacityExceededException
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
			byte[] data = deserialise();
			
			// Input stream:
			ByteArrayInputStream rawIn = new ByteArrayInputStream(data);
			in = new BitInputStream(rawIn);
			
			// Read schema ID & version:
			int schemaID = (int) Schema.SCHEMA_ID_FIELD.read(in);
			int schemaVersion = (int) Schema.SCHEMA_VERSION_FIELD.read(in);
			//if(schemaToUse != null)
			//	System.out.println("Using provided schema (ID: " + schemaToUse.getID() + "; version: " + schemaToUse.getVersion() + ") instead of the one indicated by the transmission (ID: " + schemaID + "; version: " + schemaVersion + ").");
			
			// Look-up schema unless one was provided:
			schema = (schemaToUse == null ? modelProvider.getSchema(schemaID, schemaVersion) : schemaToUse);
			if(schema == null)
				throw new IllegalStateException("Cannot decode message because schema (ID: " + schemaID + "; version: " + schemaVersion + ") is unknown");
			
			// Look-up settings & columns to factor out:
			settings = (settingsToUse == null ? modelProvider.getSettingsFor(schema) : settingsToUse);
			setColumnsToFactorOut(modelProvider.getFactoredOutColumnsFor(schema));
			
			// Read encrypted, compressed & encoded records:
			data = in.readBytes(in.available());
			
			// Decrypt records
			data = decrypt(data);
			
			// Decompress records
			data = decompress(data);
			
			// Decode records
			//System.out.println("Decoding records from: " + data.length + " bytes");
			//System.out.println("Hash: " + BinaryHelpers.toHexadecimealString(Hashing.getSHA256Hash(data)));
			decodeRecords(data);
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
	
	protected abstract void serialise(byte[] data) throws TransmissionCapacityExceededException;
	
	protected abstract byte[] deserialise() throws IOException;

	public float getCompressionRatio()
	{
		return compressionRatio;
	}
	
}
