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

package uk.ac.ucl.excites.sapelli.transmission.modes.http;

import java.io.IOException;

import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.transmission.Payload;
import uk.ac.ucl.excites.sapelli.transmission.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.control.TransmissionController;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

/**
 * @author mstevens
 *
 * http://stackoverflow.com/questions/3049626/sending-binary-data-via-post-on-android
 * http://stackoverflow.com/questions/20508788/do-i-need-content-type-application-octet-stream-for-file-download
 * http://stackoverflow.com/questions/4047731/send-imagejpg-via-httppost-from-android-to-servletwebserver
 * Use mimi type: "application/octet-stream"
 */
public class HTTPTransmission extends Transmission
{
	
	public static final int MAX_BODY_SIZE = 4096; // bytes (TODO determine a good value)
	
	private String serverURL;
	private byte[] body;
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param client
	 * @param serverURL
	 * @param payload
	 */
	public HTTPTransmission(TransmissionClient client, String serverURL, Payload payload)
	{
		super(client, payload);
		this.serverURL = serverURL;
	}
	
	/**
	 * To be called on the receiving side.
	 * 
	 * @param client
	 * @param sendingSideID
	 * @param payloadHash
	 * @param body
	 * @param receivedAt
	 */
	public HTTPTransmission(TransmissionClient client, int sendingSideID, int payloadHash, byte[] body, TimeStamp receivedAt)
	{
		super(client, sendingSideID, payloadHash);
		this.body = body;
		setReceivedAt(receivedAt);
	}
	
	/**
	 * Called when retrieving transmission from database
	 * 
	 * @param client
	 * @param localID
	 * @param remoteID - may be null
	 * @param payloadHash
	 * @param sentAt - may be null
	 * @param receivedAt - may be null
	 * @param serverURL - may be null on receiving side
	 * @param body
	 */
	public HTTPTransmission(TransmissionClient client, int localID, Integer remoteID, int payloadHash, TimeStamp sentAt, TimeStamp receivedAt, String serverURL, byte[] body) 
	{
		super(client, localID, remoteID, payloadHash, sentAt, receivedAt);
		this.serverURL = serverURL;
		this.body = body;
	}
	
	@Override
	protected void doSend(TransmissionController transmissionSender)
	{
		//HTTPClient client = sender.getHTTPClient();
		// TODO HTTP sending
	}

	@Override
	protected void wrap(BitArray payloadBits) throws TransmissionCapacityExceededException
	{
		byte[] payloadBytes = payloadBits.toByteArray();
 		//String serialisedData = Base64.encodeBase64String(payloadBytes);
		if(payloadBytes.length > MAX_BODY_SIZE)
			throw new TransmissionCapacityExceededException("Maximum body size (" + MAX_BODY_SIZE + "), exceeded by " + (payloadBytes.length - MAX_BODY_SIZE) + " bytes");
		this.body = payloadBytes;
	}

	@Override
	protected BitArray unwrap() throws IOException
	{
		if(body == null)
			throw new IllegalStateException("Transmission body has not been set.");
		return BitArray.FromBytes(body); //Base64.decodeBase64(body)
	}
	
	@Override
	public boolean isComplete()
	{
		return body != null;
	}
	
	public byte[] getBody()
	{
		return body;
	}
	
	/**
	 * @return the serverURL
	 */
	public String getServerURL()
	{
		return serverURL;
	}

	@Override
	protected int getMaxBodyBits()
	{
		return MAX_BODY_SIZE * Byte.SIZE;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.Transmission#canWrapCanIncreaseSize()
	 */
	@Override
	public boolean canWrapIncreaseSize()
	{
		return false;
	}

	@Override
	public Type getType()
	{
		return Type.HTTP;
	}
	
	@Override
	public void handle(Handler handler)
	{
		handler.handle(this);
	}
	
}
