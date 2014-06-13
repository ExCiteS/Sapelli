package uk.ac.ucl.excites.sapelli.transmission;

import java.io.IOException;

import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.compression.CompressorFactory;
import uk.ac.ucl.excites.sapelli.transmission.compression.CompressorFactory.Compression;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

/**
 * Payload class, represents the binary encoded content of a {@link Transmission}
 * 
 * @author mstevens
 */
public abstract class Payload
{

	// TODO Type field
	
	static public enum Type
	{
		Records,
		Files,
		Ack
		//...
	}
	
	static Payload New(Transmission transmission, Type type)
	{
		switch(type)
		{
			case Records:
				return new RecordsPayload(transmission);
			//case Ack:
			//	break;
			//case Files:
			//	break;
			default:
				throw new IllegalArgumentException("Unsupport Payload type: " + type.name());
		}
	}

	protected final Transmission transmission;
	
	public Payload(Transmission transmission)
	{
		this.transmission = transmission;
	}
	
	public abstract Type getType();

	/**
	 * @return the transmission
	 */
	public Transmission getTransmission()
	{
		return transmission;
	}
	
	public boolean isFull()
	{
		return false; //TODO (delegate to subclass?) keep bitarray as state and compare with max size?
	}
	
	protected abstract BitArray preparePayload() throws IOException, TransmissionCapacityExceededException, UnknownModelException;
	
	protected abstract void receivePayload(BitArray payloadBits) throws IncompleteTransmissionException, IOException, DecodeException;
	
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
