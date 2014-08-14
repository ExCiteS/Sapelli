/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

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
	static public final int PAYLOAD_TYPE_SIZE = 5; // bits --> max 32 different types (first 16 of which are reserved for types built into the transmission framework)
	static public final IntegerRangeMapping PAYLOAD_TYPE_FIELD = IntegerRangeMapping.ForSize(0, PAYLOAD_TYPE_SIZE); // unsigned(!) 5 bit integer
	
	static public enum BuiltinType
	{
		Records,
		Files,
		Ack
		//... up to 16 different built-in types (ordinals 0 to 15)
	}
	
	static public final int MAX_BUILTIN_TYPES = (int) Math.pow(2, PAYLOAD_TYPE_SIZE - 1); // = 16
	
	static Payload New(BuiltinType type)
	{
		switch(type)
		{
			case Records:
				return new RecordsPayload();
			case Ack:
				return new AckPayload();
			case Files:
			default:
				throw new IllegalArgumentException("Unsupport Payload type: " + type.name());
		}
	}
	
	static Payload New(TransmissionClient client, int payloadType) throws IllegalArgumentException
	{
		// Try to instantiate Payload object for the type: 
		if(payloadType >= MAX_BUILTIN_TYPES)
			return client.newPayload(payloadType);
		else
		{
			if(payloadType < BuiltinType.values().length)
				return New(BuiltinType.values()[payloadType]);
			else
				throw new IllegalArgumentException("Unsupport Payload type: " + payloadType);
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
		
		// Serialise payload data:
		write(bitstream);
		
		// Close & return bits (no need for flush because it doesn't do anything on a BitArrayOutputStream):
		bitstream.close();
		return bitstream.toBitArray();
	}
	
	protected abstract void write(BitOutputStream bitstream) throws IOException, TransmissionCapacityExceededException, UnknownModelException;
	
	protected void deserialise(BitArray payloadBits) throws IllegalStateException, IOException, PayloadDecodeException, UnknownModelException
	{
		if(this.transmission == null)
			throw new IllegalStateException("Cannot deserialise before transmission has been set!");
		BitArrayInputStream bitstream = null;
		try
		{
			bitstream = new BitArrayInputStream(payloadBits);
			
			// Deserialise payload data:
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
