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

package uk.ac.ucl.excites.sapelli.transmission.control;

import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Source;
import uk.ac.ucl.excites.sapelli.transmission.Payload;
import uk.ac.ucl.excites.sapelli.transmission.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;
import uk.ac.ucl.excites.sapelli.transmission.modes.http.HTTPClient;
import uk.ac.ucl.excites.sapelli.transmission.modes.http.HTTPTransmission;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSAgent;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSClient;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.binary.BinarySMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.text.TextSMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.payloads.RecordsPayload;


/**
 * @author mstevens
 *
 */
public abstract class SendController
{

	private RecordStore recordStore;
	private TransmissionClient transmissionClient; 
	private TransmissionStore transmissionStore;
	
	/**
	 * @param transmissionStore
	 */
	public SendController(TransmissionClient transmissionClient)
	{
		this.transmissionClient = transmissionClient;
	}
	
	public void setTransmissionStore(TransmissionStore transmissionStore)
	{
		this.transmissionStore = transmissionStore;
	}
	
	public TransmissionStore getTransmissionStore()
	{
		return transmissionStore;
	}
	
	public void sendRecords(Model model, Correspondent receiver) throws Exception
	{
		// Query for unsent records:
		List<Record> recsToSend = recordStore.retrieveRecords(new RecordsQuery(Source.From(model))); //TODO constraints!
		
		// while we still have records to send...
		while(!recsToSend.isEmpty())
		{
			// create a new Payload:
			RecordsPayload payload = (RecordsPayload) Payload.New(Payload.BuiltinType.Records);

			// create a new Transmission:
			Transmission transmission = null;
			switch(receiver.getTransmissionType())
			{
			case BINARY_SMS:
				transmission = new BinarySMSTransmission(transmissionClient, new SMSAgent(receiver.getAddress()), payload);
				break;
			case TEXTUAL_SMS:
				transmission = new TextSMSTransmission(transmissionClient, new SMSAgent(receiver.getAddress()), payload);
				break;
			case HTTP:
				transmission = new HTTPTransmission(transmissionClient, receiver.getAddress(), payload);
			}
			
			payload.setTransmission(transmission);

			// add as many records to the Payload as possible (this call will remove from the list the records that were successfully added to the payload):
			payload.addRecords(recsToSend);
			
			//send transmission:
			transmission.send(this);
		}
	}

	public abstract SMSClient getSMSService();
	
	public abstract HTTPClient getHTTPClient();
	
}
