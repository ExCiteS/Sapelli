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

import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.modes.http.HTTPTransmission;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.binary.BinarySMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.text.TextMessage;

/**
 * @author mstevens
 *
 */
public abstract class Receiver
{

	private TransmissionStore receivedTransmissionStore;
	private TransmissionClient client;
	
	/**
	 * 
	 */
	public Receiver(TransmissionStore receivedTransmissionStore, TransmissionClient client)
	{
		this.receivedTransmissionStore = receivedTransmissionStore;
	}
	
	protected boolean receive(Transmission transmission) throws Exception
	{
		// Receive (i.e. decode) the transmission if it is complete
		if(transmission.isComplete()) // TODO maybe this should be done in Message.receivePart()?
		{
			transmission.receive();
			
			// TODO Read payload...
			
			// Delete transmission (and parts) from store:
			receivedTransmissionStore.deleteTransmission(transmission);
			
			// TODO make & send ACK
			
			return true;
		}
		else
			return false;
	}
	
	
	public void receive(BinaryMessage binSms) throws Exception
	{
		BinarySMSTransmission transmission = receivedTransmissionStore.retrieveBinarySMSTransmission(binSms.getSender(), false, binSms.getSendingSideTransmissionID(), binSms.getPayloadHash());
		if(transmission == null) // we received the the first part
			transmission = new BinarySMSTransmission(client, binSms);
		else
			transmission.receivePart(binSms);
		
		// Store/Update transmission unless it was successfully received in its entirety:
		if(!receive(transmission))
			receivedTransmissionStore.storeTransmission(transmission);
	}
	
	public void receive(TextMessage txtSms)
	{
		
	}
	
	public void receive(HTTPTransmission httpTransmission)
	{
		
	}

	public abstract boolean deleteTransmissionUponDecoding();
	
}
