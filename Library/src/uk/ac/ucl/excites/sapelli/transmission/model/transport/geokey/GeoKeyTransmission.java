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

package uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey;

import java.io.IOException;

import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.control.TransmissionController;
import uk.ac.ucl.excites.sapelli.transmission.model.Payload;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.model.content.ResponsePayload;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;

/**
 * @author mstevens
 *
 */
public class GeoKeyTransmission extends Transmission<GeoKeyServer>
{
	
	private byte[] body;
	
	/**
	 * To be called on the sending side.
	 * 
	 * @param client
	 * @param serverURL
	 * @param payload
	 */
	public GeoKeyTransmission(TransmissionClient client, GeoKeyServer server, Payload payload)
	{
		super(client, server, payload);
	}
	
	/**
	 * To be called to create a mock response.
	 * 
	 * @param responsePayload
	 */
	public GeoKeyTransmission(ResponsePayload responsePayload)
	{
		super(responsePayload);
	}
	
	/**
	 * Called when retrieving transmission from database
	 * 
	 * @param client
	 * @param server
	 * @param received
	 * @param localID
	 * @param remoteID - may be null
	 * @param payloadType - may be null
	 * @param payloadHash
	 * @param sentAt - may be null
	 * @param response
	 * @param body
	 */
	public GeoKeyTransmission(TransmissionClient client, GeoKeyServer server, boolean received, int localID, Integer remoteID, Integer payloadType, int payloadHash, TimeStamp sentAt, TimeStamp receivedAt, GeoKeyTransmission response, byte[] body) 
	{
		super(client, server, received, localID, remoteID, payloadType, payloadHash, sentAt, receivedAt, response);
		this.body = body;
	}
	
	@Override
	protected void doSend(TransmissionController controller)
	{
		controller.getGeoKeyClient().send(this);
	}

	@Override
	protected void wrap(BitArray payloadBits) throws TransmissionCapacityExceededException
	{
		byte[] payloadBytes = payloadBits.toByteArray();
 		//String serialisedData = Base64.encodeBase64String(payloadBytes);
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
	
	@Override
	protected int getMaxBodyBits()
	{
		return UNLIMITED_BODY_SIZE;
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
		return Type.GeoKey;
	}
	
	@Override
	public void handle(Handler handler)
	{
		handler.handle(this);
	}
	
}
