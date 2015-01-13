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

import uk.ac.ucl.excites.sapelli.shared.db.StoreClient;
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
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.Message;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSAgent;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSClient;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.binary.BinarySMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.text.TextMessage;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.text.TextSMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.payloads.AckPayload;
import uk.ac.ucl.excites.sapelli.transmission.payloads.RecordsPayload;

/**
 * @author mstevens, benelliott
 *
 */
public abstract class TransmissionController implements Payload.Handler, Message.Handler, StoreClient
{

	private RecordStore recordStore;
	private TransmissionStore transmissionStore;
	private TransmissionClient transmissionClient;
	
	public TransmissionController(TransmissionClient transmissionClient, TransmissionStore transmissionStore)
	{
		this.transmissionClient = transmissionClient;
		this.transmissionStore = transmissionStore;
	}

	protected boolean doReceive(Transmission transmission) throws Exception
	{	
		// Receive (i.e. decode) the transmission if it is complete
		if(transmission.isComplete()) // TODO maybe this should be done in Message.receivePart()?
		{
			try
			{
				// "Receive" the transmission (merge parts, decode, verify):
				transmission.receive();
			
				// Handle payload:
				transmission.getPayload().handle(this);
				
				// Delete transmission (and parts) from store:
				if(deleteTransmissionUponDecoding())
					transmissionStore.deleteTransmission(transmission);
				
				// TODO make & send ACK
				sendAck(transmission);
				
				return true;
			}
			catch(Exception e)
			{
				// TODO what to do here?
				
				return false;
			}
		}
		else
			return false;
	}
	

	
	public void receive(HTTPTransmission httpTransmission) throws Exception
	{
		HTTPTransmission existingTransmission = transmissionStore.retrieveHTTPTransmission(httpTransmission.getPayload().getType(), httpTransmission.getPayloadHash());
		if(existingTransmission == null)
		{
			// Store/Update transmission unless it was successfully received in its entirety: TODO HTTP transmissions will usually be received in entirety??
			if(!doReceive(httpTransmission))
				transmissionStore.storeTransmission(httpTransmission);
		}
		// else have already seen this transmission... TODO is this check necessary?
	}

	public boolean deleteTransmissionUponDecoding()
	{
		// TODO was abstract ......
		return false;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.PayloadHandler#handle(uk.ac.ucl.excites.sapelli.transmission.payloads.AckPayload)
	 */
	@Override
	public void handle(AckPayload ackPayload)
	{
		// TODO handle ACK
		
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.PayloadHandler#handle(uk.ac.ucl.excites.sapelli.transmission.payloads.RecordsPayload)
	 */
	@Override
	public void handle(RecordsPayload recordsPayload)
	{
		// TODO Store received records...
		
	}
	
	public void handle(Message msg)
	{
		msg.handle(this); // TODO forces subtype behaviour
	}

	@Override
	public void handle(BinaryMessage binSms)
	{
		BinarySMSTransmission transmission = transmissionStore.retrieveBinarySMSTransmission(binSms.getSender(), false, binSms.getSendingSideTransmissionID(), binSms.getPayloadHash());
		if(transmission == null) // we received the the first part
			transmission = new BinarySMSTransmission(transmissionClient, binSms);
		else
			transmission.receivePart(binSms);
		
		// Store/Update transmission unless it was successfully received in its entirety:
		
		try
		{
			if(!doReceive(transmission))
				transmissionStore.storeTransmission(transmission);
		}
		catch(Exception e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	@Override
	public void handle(TextMessage txtSms)
	{
		TextSMSTransmission transmission = transmissionStore.retrieveTextSMSTransmission(txtSms.getSender(), false, txtSms.getSendingSideTransmissionID(), txtSms.getPayloadHash());
		if(transmission == null) // we received the the first part
			transmission = new TextSMSTransmission(transmissionClient, txtSms);
		else
			transmission.receivePart(txtSms);
		
		// Store/Update transmission unless it was successfully received in its entirety:
		try
		{
			if(!doReceive(transmission))
				transmissionStore.storeTransmission(transmission);
		}
		catch(Exception e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}		
	}

	@Override
	public void handle(Payload customPayload, int type)
	{
		// TODO Auto-generated method stub
		
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
	
	public void sendAck(Transmission toAck)
	{
		// TODO
	}

	public abstract SMSClient getSMSService();
	
	public abstract HTTPClient getHTTPClient();
}
