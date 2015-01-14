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
import uk.ac.ucl.excites.sapelli.transmission.db.ReceivedTransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.db.SentTransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStoreProvider;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;
import uk.ac.ucl.excites.sapelli.transmission.modes.http.HTTPClient;
import uk.ac.ucl.excites.sapelli.transmission.modes.http.HTTPTransmission;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.Message;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSAgent;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSClient;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSTransmission;
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
public abstract class TransmissionController implements Payload.Handler, StoreClient
{

	private RecordStore recordStore;
	private SentTransmissionStore sentTStore;
	private ReceivedTransmissionStore receivedTStore;
	private TransmissionClient transmissionClient;
	
	public TransmissionController(TransmissionClient transmissionClient, TransmissionStoreProvider transmissionsStoreProvider) throws Exception
	{
		this.transmissionClient = transmissionClient;
		this.sentTStore = transmissionsStoreProvider.getSentTransmissionStore(this);
		this.receivedTStore = transmissionsStoreProvider.getReceivedTransmissionStore(this);
	}
	
	public boolean deleteTransmissionUponDecoding()
	{
		// TODO was abstract ......
		return false;
	}
	
	public abstract SMSClient getSMSService();
	
	public abstract HTTPClient getHTTPClient();
	
	// ================= RECEIVE =================

	/**
	 * Method that does most of the shared busywork for receiving Transmissions (if complete, read contents and act on them).
	 * @param transmission the transmission that has been received
	 * @return true if the transmission was successfully "acted on" i.e. it was complete and no errors occured when reading it
	 */
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
					receivedTStore.deleteTransmission(transmission);
				
				// make & send ACK
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
	
	// ----- "handle"/"receive" methods for different transmission types:
	
	public void receive(HTTPTransmission httpTransmission) throws Exception
	{
		HTTPTransmission existingTransmission = receivedTStore.retrieveHTTPTransmission(httpTransmission.getPayload().getType(), httpTransmission.getPayloadHash());
		if(existingTransmission == null)
		{
			// Store/Update transmission unless it was successfully received in its entirety: TODO HTTP transmissions will usually be received in entirety??
			if(!doReceive(httpTransmission))
				receivedTStore.storeTransmission(httpTransmission);
		}
		// else have already seen this transmission... TODO is this check necessary?
	}

	/**
	 * @param msg
	 */
	public void receiveSMS(Message msg)
	{
		try
		{
			SMSReceiver receiver = new SMSReceiver(msg);
			SMSTransmission<?> transmission = receiver.transmission;
			
			if(transmission.getCurrentNumberOfParts() > 1)
			{
				// TODO cancel scheduled resend request
			}

			// Store/Update transmission unless it was successfully received in its entirety:		
			if(!doReceive(transmission))
			{
				// Transmission incomplete, waiting for more parts
				receivedTStore.storeTransmission(transmission);
				// TODO schedule resend request
			}	
		}
		catch(Exception e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}


	// ----- "handle" methods for different payload types (called once the transmission is complete):
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.PayloadHandler#handle(uk.ac.ucl.excites.sapelli.transmission.payloads.AckPayload)
	 */
	@Override
	public void handle(AckPayload ackPayload)
	{
			
		// TODO find appropriate "in-flight" transmission (from DB?), and mark it as ACKed by setting the receivedAt time to ackPayload.getSubjectReceivedAt()
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.PayloadHandler#handle(uk.ac.ucl.excites.sapelli.transmission.payloads.RecordsPayload)
	 */
	@Override
	public void handle(RecordsPayload recordsPayload)
	{
		// TODO Store received records...
		
	}

	@Override
	public void handle(Payload customPayload, int type)
	{
		// TODO Auto-generated method stub
		
	}
	
	// ================= SEND =================
	
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
	
	public void sendAck(Transmission toAck) throws Exception
	{
		AckPayload payload = new AckPayload(toAck);
		Transmission ackTransmission;
		
		switch(toAck.getType())
		{
		case BINARY_SMS:
			ackTransmission = new BinarySMSTransmission(transmissionClient, ((BinarySMSTransmission) toAck).getSender(), payload);
			break;
		case TEXTUAL_SMS:
			ackTransmission = new TextSMSTransmission(transmissionClient, ((TextSMSTransmission) toAck).getSender(), payload);
			break;
		default: // HTTP:
			ackTransmission = new HTTPTransmission(transmissionClient, ((HTTPTransmission) toAck).getSenderURL(), payload);
		}
		payload.setTransmission(ackTransmission);
		ackTransmission.send(this);
	}
	
	// TODO send custom payload?
	
	
	private class SMSReceiver implements Message.Handler
	{
		
		public SMSTransmission<?> transmission;
		
		public SMSReceiver(Message smsMsg) throws Exception
		{
			smsMsg.handle(this);
		}
		
		@Override
		public void handle(BinaryMessage binSms)
		{
			BinarySMSTransmission t = receivedTStore.retrieveBinarySMSTransmission(binSms.getSender(), binSms.getSendingSideTransmissionID(), binSms.getPayloadHash());
			if(t == null) // we received the the first part
				t = new BinarySMSTransmission(transmissionClient, binSms);
			else
				t.receivePart(binSms);
			transmission = t;
		}

		@Override
		public void handle(TextMessage txtSms)
		{
			TextSMSTransmission t = receivedTStore.retrieveTextSMSTransmission(txtSms.getSender(), txtSms.getSendingSideTransmissionID(), txtSms.getPayloadHash());
			if(t == null) // we received the the first part
				t = new TextSMSTransmission(transmissionClient, txtSms);
			else
				t.receivePart(txtSms);
			transmission = t;
		}
		
	}

}
