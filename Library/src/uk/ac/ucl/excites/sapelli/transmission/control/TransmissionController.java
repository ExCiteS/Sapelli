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

import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Source;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.db.ReceivedTransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.db.SentTransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;
import uk.ac.ucl.excites.sapelli.transmission.model.Payload;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.model.content.AckPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.RecordsPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.ResendRequestPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.http.HTTPClient;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.http.HTTPTransmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.Message;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSCorrespondent;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSSender;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.binary.BinarySMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.text.TextMessage;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.text.TextSMSTransmission;

/**
 * @author mstevens, benelliott
 *
 */
public abstract class TransmissionController implements StoreHandle.StoreUser
{
	
	static public final String UNKNOWN_CORRESPONDENT_NAME = "anonymous";

	// Client:
	private TransmissionClient transmissionClient;
	
	// Stores:
	protected final RecordStore recordStore;
	protected final SentTransmissionStore sentTStore;
	protected final ReceivedTransmissionStore receivedTStore;
	
	// Handlers:
	private final SMSReceiver smsReceiver = new SMSReceiver();
	private final PayloadReceiver payloadReceiver;
	
	public TransmissionController(TransmissionClient client) throws DBException
	{
		// Client:
		this.transmissionClient = client;
		
		// Stores:
		this.recordStore = client.recordStoreHandle.getStore(this);
		this.sentTStore = client.sentTransmissionStoreHandle.getStore(this);
		this.receivedTStore = client.receivedTransmissionStoreHandle.getStore(this);

		// Payload receiver:
		PayloadReceiver customPayloadReceiver = client.getCustomPayloadReceiver();
		this.payloadReceiver =	customPayloadReceiver != null ?
									customPayloadReceiver :
									new DefaultPayloadReceiver();
		
		// TODO log!!!
	}
	
	public boolean deleteTransmissionUponDecoding()
	{
		// TODO was abstract ......
		return false;
	}
	
	public abstract SMSSender getSMSService();
	
	public abstract HTTPClient getHTTPClient();
	
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
			Transmission<?> transmission = createOutgoingTransmission(payload, receiver);

			// add as many records to the Payload as possible (this call will remove from the list the records that were successfully added to the payload):
			payload.addRecords(recsToSend);
			
			//send transmission:
			storeAndSend(transmission);
		}
	}
	
	protected Transmission<?> createOutgoingTransmission(Payload payload, Correspondent receiver)
	{
		switch(receiver.getTransmissionType())
		{
			case BINARY_SMS:
				return new BinarySMSTransmission(transmissionClient, (SMSCorrespondent) receiver, payload);
			case TEXTUAL_SMS:
				return  new TextSMSTransmission(transmissionClient, (SMSCorrespondent) receiver, payload);
			//case HTTP:
			//	return  new HTTPTransmission(transmissionClient, receiver, payload); // TODO !!!
			default:
				System.err.println("Unsupported transmission type: " + receiver.getTransmissionType());
				return null;
		}
	}

	public void sendResendRequests() throws Exception
	{
		// Query for incomplete SMSTransmissions:
		List<SMSTransmission<?>> incompleteSMSTs = receivedTStore.getIncompleteSMSTransmissions(SMSTransmission.RESEND_REQUEST_TIMEOUT_MILLIS);
		
		// TODO Log: found x incomplete transmissions 
		
		for(SMSTransmission<?> incomplete : incompleteSMSTs)
			storeAndSend(createOutgoingTransmission(new ResendRequestPayload(incomplete), incomplete.getCorrespondent()));
		
		// TODO rememeber that a resend req was sent ...
	}
	
	private void storeAndSend(Transmission<?> transmission) throws Exception
	{
		// store in "in-flight transmissions" schema:
		sentTStore.store(transmission);
		// actually send the transmission:
		transmission.send(this);
	}
	
	// TODO send custom payload?
	
	
	// ================= RECEIVE =================

	/**
	 * Method that does most of the shared busywork for receiving Transmissions (if complete, read contents and act on them).
	 * @param transmission the transmission that has been received
	 * @return true if the transmission was successfully "acted on" i.e. it was complete and no errors occured when reading it
	 */
	protected boolean doReceive(Transmission<?> transmission) throws Exception
	{	
		// Receive (i.e. decode) the transmission if it is complete
		if(transmission.isComplete()) // TODO maybe this should be done in Message.receivePart()?
		{
			try
			{
				// "Receive" the transmission (merge parts, decode, verify):
				transmission.receive();
			
				// Handle payload (including ACK, since this is payload-dependent):
				payloadReceiver.receive(transmission.getPayload());
				
				// Acknowledge reception if needed
				if(transmission.getPayload().acknowledgeReception() /*&& transmission.getCorrespondent().wantsAck() TODO */)
				{
					// TODO this won't work for http? an http response is not quite like a sending a transmission, right?
					storeAndSend(createOutgoingTransmission(new AckPayload(transmission), transmission.getCorrespondent()));
				}
				
				// Delete transmission (and parts) from store:
				if(deleteTransmissionUponDecoding())
					receivedTStore.deleteTransmission(transmission);
				
				return true;
			}
			catch(Exception e)
			{
				throw new Exception("Exception when trying to receive supposedly complete transmission", e);
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
				receivedTStore.store(httpTransmission);
		}
		// else have already seen this transmission... TODO is this check necessary?
	}
	
	/**
	 * @param phoneNumber
	 * @param binarySMS
	 * @return
	 * @throws Exception
	 */
	public SMSCorrespondent getSendingCorrespondentFor(String phoneNumber, boolean binarySMS) throws Exception
	{
		SMSCorrespondent corr = receivedTStore.retrieveSMSCorrespondent(phoneNumber, binarySMS);
		if(corr == null)
		{
			corr = new SMSCorrespondent(UNKNOWN_CORRESPONDENT_NAME, phoneNumber, binarySMS);
			receivedTStore.store(corr);
		}
		return corr;
	}

	/**
	 * @param msg
	 */
	public void receiveSMS(Message msg)
	{
		try
		{
			smsReceiver.receive(msg);			
			// Store/Update transmission unless it was successfully received in its entirety:
			
			// TODO also store if complete?
			
			if(!doReceive(smsReceiver.transmission))
			{
				// Transmission incomplete, waiting for more parts
				receivedTStore.store(smsReceiver.transmission);
			}	
		}
		catch(Exception e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	@Override
	public void finalize()
	{
		discard();
	}
	
	public void discard()
	{
		transmissionClient.recordStoreHandle.doneUsing(this);
		transmissionClient.sentTransmissionStoreHandle.doneUsing(this);
		transmissionClient.receivedTransmissionStoreHandle.doneUsing(this);
	}
	
	/**
	 * Helper class to handle incoming SMS messages
	 * 
	 * @author mstevens
	 */
	private class SMSReceiver implements Message.Handler
	{
		
		public SMSTransmission<?> transmission;
		
		public void receive(Message smsMsg) throws Exception
		{
			transmission = null; // wipe previous transmission !!!
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
	
	/**
	 * Helper class with "handle" methods for different payload types (called once the transmission is complete)
	 * 
	 * @author benelliott, mstevens
	 */
	public abstract class PayloadReceiver implements Payload.Handler
	{
		
		/**
		 * Receive/decode payload
		 * 
		 * @param payload
		 * @throws Exception
		 */
		public void receive(Payload payload) throws Exception
		{
			payload.handle(this);
		}
		
		@Override
		public void handle(AckPayload ack) throws Exception
		{
			// find appropriate "in-flight" transmission and mark it as ACKed by setting the receivedAt time to ackPayload.getSubjectReceivedAt()
			Transmission<?> subject = sentTStore.retrieveTransmissionFor(ack.getSubjectSenderSideID(), ack.getSubjectPayloadHash());
			if(subject == null)
			{	// TODO log to file
				System.err.println("No matching transmission (ID " + ack.getSubjectSenderSideID() + ") found in the database for acknowledgement from sender " + ack.getTransmission().getCorrespondent());
				return;
			}
			
			// Mark subject as received and update in database:
			subject.setReceivedAt(ack.getSubjectReceivedAt());
			sentTStore.store(subject);
		}
		
		@Override
		public void handle(ResendRequestPayload resendReq) throws Exception
		{
			// get Transmission object from store - will definitely be an SMSTransmission since resend requests are only concerned with SMS (at least for now)
			SMSTransmission<?> subject = ((SMSTransmission<?>) sentTStore.retrieveTransmissionFor(resendReq.getSubjectSenderSideID(), resendReq.getSubjectPayloadHash()));
			if(subject == null)
			{	// TODO log to file
				System.err.println("No matching transmission (ID " + resendReq.getSubjectSenderSideID() + ") found in the database for resend request from sender " + resendReq.getTransmission().getCorrespondent());
				return;
			}
			
			// Resend requested parts:
			for(Integer partNumber : resendReq.getRequestedPartNumbers()) 
				subject.resend(TransmissionController.this, partNumber);
		}

		@Override
		public void handle(RecordsPayload recordsPayload) throws Exception
		{
			try
			{
				// Store received records...
				recordStore.store(recordsPayload.getRecords());
			}
			catch (Exception e)
			{
				throw new Exception("Unable to store records that were received from transmission", e);
			}
		}
		
	}
	
	private class DefaultPayloadReceiver extends PayloadReceiver
	{

		@Override
		public void handle(Payload customPayload, int type) throws Exception
		{
			System.err.println("Receiving custom payload (type: " + type + ") not supported!");
		}
		
	}

}
