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
	
	public void receive(BinaryMessage binSms)
	{
//		BinarySMSTransmission transmission = receivedTransmissionStore.retrieveBinarySMSTransmission(binSms.getSender(), false, 0, binSms.getPayloadHash());
//		if(transmission == null)
//			transmission = new BinarySMSTransmission(binSms.getSender(), client, binSms);
		
		// store/update transmisison!
		
		
		//TODO make & send ACK
	}
	
	public void receive(TextMessage txtSms)
	{
		
	}
	
	public void receive(HTTPTransmission httpTransmission)
	{
		
	}

	public abstract boolean deleteTransmissionUponDecoding();
	
}
