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

import java.io.IOException;
import java.util.List;
import java.util.Map;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.collector.io.FileStorageException;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.Logger;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Source;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.db.ReceivedTransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.db.SentTransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;
import uk.ac.ucl.excites.sapelli.transmission.model.Payload;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.model.content.AckPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.ModelPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.ModelRequestPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.RecordsPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.ResendRequestPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.http.HTTPTransmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.Message;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSCorrespondent;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.binary.BinarySMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.text.TextMessage;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.text.TextSMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.protocol.http.HTTPClient;
import uk.ac.ucl.excites.sapelli.transmission.protocol.sms.SMSSender;

/**
 * @author mstevens, benelliott
 *
 */
public abstract class TransmissionController implements StoreHandle.StoreUser
{
	
	static public final String UNKNOWN_CORRESPONDENT_NAME = "anonymous";
	static protected final String LOG_FILENAME_PREFIX = "Transmission_";

	// Client:
	private TransmissionClient transmissionClient;
	
	// Stores:
	protected final RecordStore recordStore;
	protected final SentTransmissionStore sentTStore;
	protected final ReceivedTransmissionStore receivedTStore;
	
	// Handlers:
	private final SMSReceiver smsReceiver = new SMSReceiver();
	private final PayloadReceiver payloadReceiver;
	
	// Logger:
	protected Logger logger;
	
	public TransmissionController(TransmissionClient client, FileStorageProvider fileStorageProvider) throws DBException
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
		
		// Logger:
		try
		{
			logger = createLogger(fileStorageProvider);
		}
		catch(Exception e)
		{
			e.printStackTrace();
		}
	}
	
	/**
	 * To be called by subclass when all contructors are done
	 */
	protected void initialise()
	{
		addLogLine("Application: " + getApplicationInfo());
		addLogLine("TRANSMISSION CONTROLLER CREATED");
	}
	
	/**
	 * To be overridden
	 */
	protected Logger createLogger(FileStorageProvider fileStorageProvider) throws FileStorageException, IOException
	{
		return new Logger(fileStorageProvider.getLogsFolder(true).getAbsolutePath(), LOG_FILENAME_PREFIX + DateTime.now().toString("yyyy-mm-dd"), true);
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
		List<Record> recsToSend = recordStore.retrieveRecords(new RecordsQuery(Source.From(model))); //TODO constraints!!
		addLogLine("Records to send: " + recsToSend.size());
		// while we still have records to send...
		while(!recsToSend.isEmpty())
		{
			// create a new Payload:
			RecordsPayload payload = (RecordsPayload) Payload.New(Payload.BuiltinType.Records);

			// create a new Transmission:
			Transmission<?> transmission = createOutgoingTransmission(payload, receiver);

			// add as many records to the Payload as possible (this call will remove from the list the records that were successfully added to the payload):
			addLogLine("Trying to add "+recsToSend.size()+" records to payload...");
			payload.addRecords(recsToSend);
			addLogLine("Records that weren't added: "+recsToSend.size()+"; payload says it has "+payload.getNumberOfRecords());
			//send transmission:
			storeAndSend(transmission);
			
			
			// TODO mark records as "sent" (do here rather than ACK? depends on resend timeout vs. send new records timeout; semantics of resending transmission vs. records)
		}
	}
	
	protected Transmission<?> createOutgoingTransmission(Payload payload, Correspondent receiver)
	{
		switch(receiver.getTransmissionType())
		{
			case BINARY_SMS:
				return new BinarySMSTransmission(transmissionClient, (SMSCorrespondent) receiver, payload);
			case TEXTUAL_SMS:
				return new TextSMSTransmission(transmissionClient, (SMSCorrespondent) receiver, payload);
			//case HTTP:
			//	return  new HTTPTransmission(transmissionClient, receiver, payload); // TODO !!!
			default:
				System.err.println("Unsupported transmission type: " + receiver.getTransmissionType());
				return null;
		}
	}
	
	private void storeAndSend(Transmission<?> transmission) throws Exception
	{
		addLogLine("OUTGOING TRANSMISSION", transmission.getType().toString(), "PAYLOAD: " + transmission.getPayload().getType(), "TO: "+transmission.getCorrespondent().getName()+" ("+transmission.getCorrespondent().getAddress()+")");
		
		// Prepare transmission for storage & sending:
		transmission.prepare();
		
		// Store "in-flight transmissions" to get local ID:
		sentTStore.store(transmission); // update record now that payload hash has been computed
		
		// actually send the transmission:
		transmission.send(this);
	}
	
	public void updateSentTransmission(Transmission<?> transmission)
	{
		try
		{
			sentTStore.store(transmission); // TODO only update not insert?
		}
		catch(Exception e)
		{
			logger.addLine("Failed to update sent transmission");
			e.printStackTrace();
		}
	}
	
	// TODO send custom payload?
		
	// ================= RECEIVE =================

	/**
	 * Method that does most of the work for receiving Transmissions (if complete, read contents and act on them).
	 * 
	 * @param transmission the transmission that has been received, it is assumed to be complete!
	 * @return true if the transmission was successfully "acted on" i.e. it was complete and no errors occurred when reading it
	 */
	protected boolean doReceive(Transmission<?> transmission) throws Exception
	{	
		addLogLine(	"INCOMING", "Transmission", transmission.getType().toString(),
					"From: " + transmission.getCorrespondent());

		// Receive (i.e. decode) the transmission:
		try
		{
			// "Receive" the transmission (merge parts, decode, verify):
			transmission.receive();
		
			// Handle/receive the payload:
			payloadReceiver.receive(transmission, transmission.getPayload()); // TODO call payload.deserialise() from here instad of transmission.receive()??
			
			// Acknowledge reception if needed
			if(transmission.getPayload().acknowledgeReception() /*&& transmission.getCorrespondent().wantsAck() TODO */)
			{
				try
				{	// TODO this won't work for http? an http response is not quite like a sending a transmission, right?
					storeAndSend(createOutgoingTransmission(new AckPayload(transmission), transmission.getCorrespondent()));
				}
				catch(Exception e)
				{
					throw new Exception("Error upon sending ACK", e);
				}
			}
			
			// Delete transmission (and parts) from store:
			if(deleteTransmissionUponDecoding())
				receivedTStore.deleteTransmission(transmission);
			
			return true;
		}
		catch(UnknownModelException e)
		{
			// TODO send model request payload, would make more sense to do this in the payload receiver but the exception is thrown from transmission.receive()
			return false;
		}
		catch(Exception e)
		{
			throw new Exception("Exception when trying to receive/decode transmission", e);
		}
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
		// Try to find sender:
		SMSCorrespondent corr = receivedTStore.retrieveSMSCorrespondent(phoneNumber, binarySMS);
		if(corr == null)
		{
			// Try to find matching receiver:
			corr = sentTStore.retrieveSMSCorrespondent(phoneNumber, binarySMS);
			// Still nothing -> make new correspondent
			if(corr == null)
				corr = new SMSCorrespondent(UNKNOWN_CORRESPONDENT_NAME, phoneNumber, binarySMS);
			receivedTStore.store(corr); // also store if using receiver, send/receive correspondents are in separate tables (TODO normalise?)
		}
		return corr;
	}

	/**
	 * @param msg
	 */
	public void receiveSMS(Message msg) throws Exception
	{
		try
		{
			// Receive the message:
			smsReceiver.receive(msg);
			SMSTransmission<?> smsTrans = smsReceiver.transmission;
			
			// Store transmission:
			receivedTStore.store(smsTrans);
			
			// Try receiving the transmission:
			if(!smsTrans.isComplete())
			{	// Transmission incomplete, we need to wait for more parts and schedule a resend request (in case they don't come):
				addLogLine("INCOMING", "Transmission incomplete (got " + smsTrans.getCurrentNumberOfParts() + "/" + smsTrans.getTotalNumberOfParts() + " parts) waiting for others...");
				scheduleSMSResendRequest(smsTrans.getLocalID(), smsTrans.getNextResendRequestSendingTime());
			}	
			else
			{	// Transmission is complete ...
				if(smsTrans.getTotalNumberOfParts() > 1) // ... and consisted of more than one part:
					cancelSMSResendRequest(smsTrans.getLocalID()); // cancel any pending resend requests
				// Further (payload) receiving work:
				doReceive(smsReceiver.transmission);
			}
		}
		catch(Exception e)
		{
			addLogLine("EXCEPTION", "Upon SMS message reception", ExceptionHelpers.getMessageAndCause(e));
			throw e;
		}
	}

	/**
	 * @param localID local ID of an incomplete SMSTransmission
	 * @return whether or not future resend requests may needed for this transmission 
	 * @throws Exception
	 */
	public void sendSMSResendRequest(int localID) throws Exception
	{
		// Query for the transmission:
		Transmission<?> trans = receivedTStore.retrieveTransmissionForID(localID);
		
		// Check if it makes sense to send the request...
		if(trans == null || !( trans instanceof SMSTransmission) || trans.isComplete())
			return;

		// Cast to SMSTransmission:
		final SMSTransmission<?> smsTrans = (SMSTransmission<?>) trans;
		
		// Further checks...
		TimeStamp sendReqAt = smsTrans.getNextResendRequestSendingTime();
		if(sendReqAt == null || sendReqAt.isAfter(TimeStamp.now()))
			return; // either no more reqs are allowed, or it is too early to send the next one (shouldn't happen)
		
		addLogLine("PREPARING", "Outgoing resend request for incomplete transmission with local ID: " + localID);
		
		// Send request:
		storeAndSend(createOutgoingTransmission(new ResendRequestPayload(smsTrans, this), smsTrans.getCorrespondent()));
	}
	
	/**
	 * Schedules resend requests for all incomplete SMSTransmissions
	 * To be called upon device boot.
	 * 
	 * @return whether any requests were scheduled
	 * @throws Exception
	 */
	public boolean scheduleSMSResendRequests() throws Exception
	{
		// Query for incomplete SMSTransmissions:
		List<SMSTransmission<?>> incompleteSMSTs = receivedTStore.getIncompleteSMSTransmissions();		
		addLogLine("Incomplete SMS transmissions found: " + incompleteSMSTs.size());
		
		boolean atLeast1 = false;
		for(SMSTransmission<?> incomplete : incompleteSMSTs)
		{
			TimeStamp sendReqAt = incomplete.getNextResendRequestSendingTime();
			if(sendReqAt != null)
			{
				scheduleSMSResendRequest(incomplete.getLocalID(), sendReqAt);
				atLeast1 = true;
			}
		}
		return atLeast1;
	}
	
	/**
	 * @param localID local ID of an incomplete SMSTransmission
	 * @param time at which to send the request
	 */
	public abstract void scheduleSMSResendRequest(int localID, TimeStamp time);
	
	/**
	 * @param localID local ID of an incomplete SMSTransmission
	 */
	protected abstract void cancelSMSResendRequest(int localID);
	
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
			log(binSms, true);
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
			log(txtSms, false);
			TextSMSTransmission t = receivedTStore.retrieveTextSMSTransmission(txtSms.getSender(), txtSms.getSendingSideTransmissionID(), txtSms.getPayloadHash());
			if(t == null) // we received the the first part
				t = new TextSMSTransmission(transmissionClient, txtSms);
			else
				t.receivePart(txtSms);
			transmission = t;
		}
		
		private void log(Message msg, boolean binary)
		{
			addLogLine(	"INCOMING", "SMS", binary ? "Binary" : "Text",
						"SendingSideTransmissionID: " + msg.getSendingSideTransmissionID(),
						"Part: " + msg.getPartNumber() + "/" + msg.getTotalParts(),
						"From: "+ msg.getSender().getName() + " (" + msg.getSender().getAddress() + ")");
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
		 * @param transmission
		 * @param payload
		 * @throws Exception
		 */
		public void receive(Transmission<?> transmission, Payload payload) throws Exception
		{
			payload.handle(this);
		}
		
		@Override
		public void handle(AckPayload ack) throws Exception
		{
			// find appropriate "in-flight" transmission and mark it as ACKed by setting the receivedAt time to ackPayload.getSubjectReceivedAt()
			Transmission<?> subject = sentTStore.retrieveTransmissionFor(ack.getSubjectSenderSideID(), ack.getSubjectPayloadHash());
			
			addLogLine(	"INCOMING", "Payload", "ACK",
						"Subject local ID: " + ack.getSubjectSenderSideID(),
						"Subject hash: " + ack.getSubjectPayloadHash(),
						"Subject found: " + (subject != null));
			if(subject != null)
			{
				// Mark subject as received and update in database:
				subject.setReceivedAt(ack.getSubjectReceivedAt());
				if(ack.getSubjectReceiverSideID() != null)
					subject.setRemoteID(ack.getSubjectReceiverSideID()); // mainly for debugging
				sentTStore.store(subject);
			}
			else
				System.err.println("No matching transmission (ID " + ack.getSubjectSenderSideID() + "; payload hash: " + ack.getSubjectPayloadHash() + " ) found in the database for acknowledgement from sender " + ack.getTransmission().getCorrespondent());
		}
		
		@Override
		public void handle(ResendRequestPayload resendReq) throws Exception
		{
			// get Transmission object from store - will definitely be an SMSTransmission since resend requests are only concerned with SMS (at least for now)
			SMSTransmission<?> subject = ((SMSTransmission<?>) sentTStore.retrieveTransmissionFor(resendReq.getSubjectSenderSideID(), resendReq.getSubjectPayloadHash()));
			
			addLogLine(	"INCOMING", "Payload", "ResendReq",
						"Subject local ID: " + resendReq.getSubjectSenderSideID(),
						"Subject hash: " + resendReq.getSubjectPayloadHash(),
						"Requested parts: " + StringUtils.join(resendReq.getRequestedPartNumbers(), ", "),
						"Subject found: " + (subject != null));
			if(subject != null) // subject is known ...
			{	
				if(!subject.isReceived()) // ... and we haven't received a ACK yet (check just in case):
					// Resend requested parts:
					for(Integer partNumber : resendReq.getRequestedPartNumbers())
						subject.resend(TransmissionController.this, partNumber);
			}
			else
				System.err.println("No matching transmission (ID " + resendReq.getSubjectSenderSideID() + "; payload hash: " + resendReq.getSubjectPayloadHash() + " ) found in the database for acknowledgement from sender " + resendReq.getTransmission().getCorrespondent());			
		}

		@Override
		public void handle(RecordsPayload recordsPayload) throws Exception
		{
			if(logger != null)
			{
				StringBuilder schemataString = new StringBuilder();
				Map<Schema, List<Record>> recordsBySchema = recordsPayload.getRecordsBySchema();
				for (Schema schema : recordsPayload.getSchemata())
				{
					schemataString.append(schema.getName());
					schemataString.append(" (");
					schemataString.append((recordsBySchema.get(schema) != null) ? recordsBySchema.get(schema).size() : -1);
					schemataString.append(")");
					schemataString.append(",");
				}
				logger.addLine("INCOMING RECORDS", "TOTAL: "+recordsPayload.getNumberOfRecords(), "SCHEMATA: "+schemataString.toString());
			}
			
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
		
		@Override
		public void handle(ModelPayload projectModelPayload) throws Exception
		{
			// TODO
			addLogLine("INCOMING MODEL", "ID: "+projectModelPayload.getModel().getID(), "NAME: "+projectModelPayload.getModel().getName());
			// add model from payload
			// try to decode records from unknown model
		}
		
		@Override
		public void handle(ModelRequestPayload modelRequestPayload) throws Exception
		{
			// TODO
			addLogLine("INCOMING MODEL REQUEST", "ID: "+modelRequestPayload.getUnknownModelID());
			// look for requested model
			
			// create projectModelPayload and send
		}
		
	}
	
	private class DefaultPayloadReceiver extends PayloadReceiver
	{

		@Override
		public void handle(Payload customPayload, int type) throws Exception
		{
			addLogLine("INCOMING CUSTOM", "TRANS ID: "+customPayload.getTransmission().getLocalID());
			System.err.println("Receiving custom payload (type: " + type + ") not supported!");
		}
		
	}
	
	protected abstract String getApplicationInfo();
	
	public void addLogLine(String... fields)
	{
		if(logger != null)
			logger.addLine(fields);
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
	
}
