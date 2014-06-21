package uk.ac.ucl.excites.sapelli.transmission;

import java.io.IOException;

import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.shared.io.BitArrayInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitArrayOutputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.compression.CompressorFactory;
import uk.ac.ucl.excites.sapelli.transmission.compression.CompressorFactory.Compression;
import uk.ac.ucl.excites.sapelli.transmission.payloads.AckPayload;
import uk.ac.ucl.excites.sapelli.transmission.payloads.RecordsPayload;
import uk.ac.ucl.excites.sapelli.transmission.util.PayloadDecodeException;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

/**
 * Payload class, represents the binary encoded content of a {@link Transmission}.
 * 
 * @author mstevens
 */
public abstract class Payload
{

	// STATICS-------------------------------------------------------
	static public final int PAYLOAD_TYPE_SIZE = 5; // bits
	static public final IntegerRangeMapping PAYLOAD_TYPE_FIELD = IntegerRangeMapping.ForSize(0, PAYLOAD_TYPE_SIZE); // unsigned(!) 5 bit integer
	
	static public enum BuiltinType
	{
		Records,
		Files,
		Heartbeat,
		Ack
		//... up to 16 different built-in types (ordinals 0 to 15)
	}
	
	static public final int MAX_BUILTIN_TYPES = 16;
	
	static Payload New(BuiltinType type)
	{
		switch(type)
		{
			case Records:
				return new RecordsPayload();
			case Ack:
				return new AckPayload();
			case Files:
			case Heartbeat:
			default:
				throw new IllegalArgumentException("Unsupport Payload type: " + type.name());
		}
	}
	
	static Payload New(TransmissionClient client, BitArray payloadBits) throws IOException
	{
		// Read payload type:
		int type = (int) PAYLOAD_TYPE_FIELD.read(payloadBits, 0);
		// Try to instantiate Payload object for the type: 
		if(type >= MAX_BUILTIN_TYPES)
			return client.newPayload(type);
		else
		{
			if(type < BuiltinType.values().length)
				return New(BuiltinType.values()[type]);
			else
				throw new IllegalArgumentException("Unsupport Payload type: " + type);
		}
	}
	
	// DYNAMICS------------------------------------------------------
	protected Transmission transmission;

	public abstract int getType();

	/**
	 * @return the transmission
	 */
	public Transmission getTransmission()
	{
		return transmission;
	}
	
	public void setTransmission(Transmission transmission)
	{
		if(this.transmission != null && this.transmission != transmission)
			throw new IllegalStateException("Transmission cannot be changed once it has been set!");
		this.transmission = transmission;
	}
	
	public boolean isTansmissionSet()
	{
		return transmission != null;
	}
	
	public BitArray serialise() throws IOException, TransmissionCapacityExceededException, UnknownModelException
	{
		if(this.transmission == null)
			throw new IllegalStateException("Cannot serialise before transmission has been set!");
	
		BitArrayOutputStream bitstream = new BitArrayOutputStream();
		
		// Write payload type:
		PAYLOAD_TYPE_FIELD.write(getType(), bitstream);
		
		// Write payload payload:
		write(bitstream);
		
		// Flush, close & get bits:
		bitstream.flush();
		bitstream.close();
		BitArray bits = bitstream.toBitArray();
		
		// Capacity checks:
		if(bits.length() > transmission.getMaxPayloadBits())
			throw new TransmissionCapacityExceededException("Payload is too large for the associated transmission (size: " + bits.length() + " bits; max for transmission: " + transmission.getMaxPayloadBits() + " bits");
		// TODO transmission/message serialisation (because escaping can lead to growth in payload size in some types of transmissions) 
		
		// Return bits:
		return bits;
	}
	
	protected abstract void write(BitOutputStream bitsteam) throws IOException, TransmissionCapacityExceededException, UnknownModelException;
	
	protected void deserialise(BitArray payloadBits) throws IllegalStateException, IOException, PayloadDecodeException, UnknownModelException
	{
		if(this.transmission == null)
			throw new IllegalStateException("Cannot deserialise before transmission has been set!");
		BitArrayInputStream bitstream = null;
		try
		{
			bitstream = new BitArrayInputStream(payloadBits);
			
			// Verify payload type:
			int type = (int) PAYLOAD_TYPE_FIELD.read(bitstream); 
			if(type != getType())
				throw new IllegalStateException("Invalid payload type (" + type + "; expecting " + getType() + ")");
			
			// Deserialise payload:
			read(bitstream);
		}
		finally
		{
			if(bitstream != null)
				bitstream.close();	
		}
	}
	
	protected abstract void read(BitInputStream bitstream) throws IOException, PayloadDecodeException, UnknownModelException;
	
	protected byte[][] compress(BitArray data, Compression[] modes) throws IOException
	{
		return compress(data.toByteArray(), modes);
	}
	
	protected byte[][] compress(byte[] data, Compression[] modes) throws IOException
	{
		byte[][] result = new byte[modes.length][];
		for(int m = 0; m < modes.length; m++)
			result[m] = CompressorFactory.getCompressor(modes[m]).compress(data);
		return result;
	}

	protected byte[] decompress(byte[] compressedData, Compression mode) throws IOException
	{
		return CompressorFactory.getCompressor(mode).decompress(compressedData);
	}
	
	protected byte[] encrypt(byte[] data, EncryptionSettings.Key key) throws IOException
	{
		//TODO encryption	
		return data;
	}

	protected byte[] decrypt(byte[] data, EncryptionSettings[] settings) throws IOException
	{
		//TODO decryption
		return data;
	}
	
}
