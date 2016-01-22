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

package uk.ac.ucl.excites.sapelli.transmission.model;

import java.io.IOException;

import uk.ac.ucl.excites.sapelli.shared.compression.CompressorFactory;
import uk.ac.ucl.excites.sapelli.shared.compression.CompressorFactory.Compression;
import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.shared.io.BitArrayInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitArrayOutputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.shared.io.StreamHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.model.content.AckPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.ModelQueryPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.ModelRequestPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.RecordsPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.ResendRequestPayload;
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
		Ack,
		ResendRequest,
		ModelQuery,
		ModelRequest,
		Model,
		//... up to 16 different built-in types (ordinals 0 to 15)
	}
	
	static public final int MAX_BUILTIN_TYPES = (int) Math.pow(2, PAYLOAD_TYPE_SIZE - 1); // = 16
	
	static public Payload New(BuiltinType type)
	{
		switch(type)
		{
			case Records:
				return new RecordsPayload();
			case Ack:
				return new AckPayload();
			case ResendRequest:
				return new ResendRequestPayload();
			case ModelQuery:
				return new ModelQueryPayload();
			case ModelRequest:
				return new ModelRequestPayload();
			case Model:
			case Files:
			default:
				throw new IllegalArgumentException("Unsupported/Unimplemented Payload type: " + type.name());
		}
	}
	
	static public Payload New(TransmissionClient client, int payloadType) throws IllegalArgumentException
	{
		// Try to instantiate Payload object for the type: 
		if(payloadType >= MAX_BUILTIN_TYPES)
			return client.createCustomPayload(payloadType);
		else
		{
			if(payloadType >= 0 && payloadType < BuiltinType.values().length)
				return New(BuiltinType.values()[payloadType]);
			else
				throw new IllegalArgumentException("Unsupport Payload type: " + payloadType);
		}
	}
	
	static public String GetPayloadTypeString(int payloadType) throws IllegalArgumentException
	{
		if(payloadType >= MAX_BUILTIN_TYPES)
			return "Custom";
		else
		{
			if(payloadType >= 0 && payloadType < BuiltinType.values().length)
				return BuiltinType.values()[payloadType].name();
			else
				throw new IllegalArgumentException("Unsupport Payload type: " + payloadType);
		}
	}
	
	/**
	 * Interface for dispatching on payload type
	 * 
	 */
	static public interface Handler
	{

		public void handle(AckPayload ackPayload) throws Exception;
		
		public void handle(RecordsPayload recordsPayload) throws Exception;
		
		public void handle(ResendRequestPayload resendRequestPayload) throws Exception;
		
		public void handle(ModelQueryPayload modelQueryPayload) throws Exception;
		
		public void handle(ModelRequestPayload modelRequestPayload) throws Exception;
		
		/**
		 * Handle method for non-built-in payload types
		 * 
		 * @param customPayload
		 * @param type
		 */
		public void handle(Payload customPayload, int type) throws Exception;
		
	}
	
	static protected byte[][] Compress(BitArray data, Compression[] modes) throws IOException
	{
		return Compress(data.toByteArray(), modes);
	}
	
	static protected byte[][] Compress(byte[] data, Compression[] modes) throws IOException
	{
		byte[][] result = new byte[modes.length][];
		for(int m = 0; m < modes.length; m++)
			result[m] = CompressorFactory.getCompressor(modes[m]).compress(data);
		return result;
	}

	static protected byte[] Decompress(byte[] compressedData, Compression mode) throws IOException
	{
		return CompressorFactory.getCompressor(mode).decompress(compressedData);
	}
	
	// DYNAMICS------------------------------------------------------
	protected Transmission<?> transmission;
	
	protected PayloadDecodeException decodeException = null;
	
	public abstract int getType();

	/**
	 * Only used on sending side, and even there if can be null for certain payloads
	 */
	private SentCallback callback;
	
	/**
	 * @return the transmission
	 */
	public Transmission<?> getTransmission()
	{
		return transmission;
	}
	
	public void setTransmission(Transmission<?> transmission)
	{
		if(this.transmission != null && this.transmission != transmission)
			throw new IllegalStateException("Transmission cannot be changed once it has been set!");
		this.transmission = transmission;
	}
	
	/**
	 * @return the callback- may be null
	 */
	public SentCallback getCallback()
	{
		return callback;
	}

	/**
	 * @param callback the callback to set
	 */
	protected void setCallback(SentCallback callback)
	{
		this.callback = callback;
	}

	public boolean isTansmissionSet()
	{
		return transmission != null;
	}
	
	public BitArray serialise() throws IOException, TransmissionCapacityExceededException
	{
		if(this.transmission == null)
			throw new IllegalStateException("Cannot serialise before transmission has been set!");
	
		BitArrayOutputStream bitstream = null;
		try
		{
			bitstream = new BitArrayOutputStream();
		
			// Serialise payload data:
			write(bitstream);
		
			// Close & return bits (no need for flush because it doesn't do anything on a BitArrayOutputStream):
			bitstream.close();
			return bitstream.toBitArray();
		}
		finally
		{
			StreamHelpers.SilentClose(bitstream);
		}
	}
	
	protected abstract void write(BitOutputStream bitstream) throws IOException, TransmissionCapacityExceededException;
	
	/**
	 * Important: this method does *not* throw exceptions but instead stores any exception which does occur as the {@link #decodeException}.
	 * 
	 * @param payloadBits
	 */
	public void deserialise(BitArray payloadBits)
	{
		decodeException = null; // wipe any previous exception 
		BitArrayInputStream bitstream = null;
		try
		{
			if(this.transmission == null)
				throw new IllegalStateException("Cannot deserialise before transmission has been set!");
			
			bitstream = new BitArrayInputStream(payloadBits);
			
			// Deserialise payload data:
			read(bitstream);
		}
		catch(Exception e)
		{
			// don't throw!
			decodeException = (e instanceof PayloadDecodeException) ? (PayloadDecodeException) e : new PayloadDecodeException(this, e);
		}
		finally
		{
			StreamHelpers.SilentClose(bitstream);
		}
	}
	
	public boolean hasDecodeException()
	{
		return decodeException != null;
	}
	
	/**
	 * @return the decodeException or {@code null}
	 */
	public PayloadDecodeException getDecodeException()
	{
		return decodeException;
	}

	protected abstract void read(BitInputStream bitstream) throws IOException, PayloadDecodeException;
	
	public abstract boolean acknowledgeReception();
	
	/**
	 * To be overridden by built-in payload types!
	 * 
	 * @param handler
	 */
	public void handle(Handler handler) throws Exception
	{
		handler.handle(this, getType()); // use generic handler method
	}
	
	/**
	 * @author mstevens
	 */
	protected class SentCallback
	{
		
		public void onSent(TimeStamp sentAt)
		{
			// does nothing by default
		}
		
	}
	
}
