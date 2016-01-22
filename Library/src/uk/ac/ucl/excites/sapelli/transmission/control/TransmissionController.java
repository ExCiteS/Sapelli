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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.joda.time.DateTime;

import com.google.i18n.phonenumbers.Phonenumber.PhoneNumber;

import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.io.FileStorageException;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.Logger;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.shared.util.TransactionalStringBuilder;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;
import uk.ac.ucl.excites.sapelli.transmission.model.Payload;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.model.content.AckPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.ModelQueryPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.ModelRequestPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.RecordsPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.ResendRequestPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.ResponsePayload;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey.GeoKeyAccount;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey.GeoKeyTransmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.Message;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSCorrespondent;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.binary.BinarySMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.text.TextMessage;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.text.TextSMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.protocol.geokey.GeoKeyClient;
import uk.ac.ucl.excites.sapelli.transmission.protocol.sms.SMSClient;
import uk.ac.ucl.excites.sapelli.transmission.util.PayloadDecodeException;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionCapacityExceededException;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionReceivingException;
import uk.ac.ucl.excites.sapelli.transmission.util.TransmissionSendingException;

/**
 * Controller class to handling all incoming / outgoing transmissions.
 * 
 * @author mstevens, benelliott
 */
public abstract class TransmissionController implements StoreHandle.StoreUser
{
	
	static protected final String LOG_FILENAME_PREFIX = "Transmission_";
	
	static public enum ModelQueryStatus
	{
		Pending,
		ModelPresent,
		ModelAbsent
	};

	// Client:
	private TransmissionClient transmissionClient;
	
	// Stores:
	protected final RecordStore recordStore;
	protected final TransmissionStore transmissionStore;
	
	// Handlers:
	private final SMSReceiver smsReceiver = new SMSReceiver();
	private final PayloadReceiver payloadReceiver;
	private final PayloadAckHandler payloadAckHandler;
	
	// Logger:
	private Logger logger;
	
	public TransmissionController(TransmissionClient client) throws DBException
	{
		// Client:
		this.transmissionClient = client;
		
		// Stores:
		this.recordStore = client.recordStoreHandle.getStore(this);
		this.transmissionStore = client.transmissionStoreHandle.getStore(this);

		// Payload receiver:
		PayloadReceiver customPayloadReceiver = client.getCustomPayloadReceiver();
		this.payloadReceiver =	customPayloadReceiver != null ?
									customPayloadReceiver :
									new DefaultPayloadReceiver();
		
		// Payload ACK handler:
		this.payloadAckHandler = new PayloadAckHandler();
	}
	
	/**
	 * To be called by subclass when all constructors are done
	 */
	public void initialise()
	{
		// Create logger:
		try
		{
			this.logger = createLogger(getLogsFolder());
		}
		catch(Exception e)
		{
			transmissionClient.logError("Could not initialise logger", e);
		}
		
		addLogLine("Application: " + getApplicationInfo());
		addLogLine("TRANSMISSION CONTROLLER CREATED");
	}
	
	/**
	 * @return an existing(!) and writable folder for storage of log files
	 */
	protected abstract File getLogsFolder() throws FileStorageException;
	
	/**
	 * May be overridden.
	 * 
	 * @param logsFolder
	 * @return
	 * @throws FileStorageException
	 * @throws IOException
	 */
	protected Logger createLogger(File logsFolder) throws FileStorageException, IOException
	{
		return new Logger(logsFolder.getAbsolutePath(), LOG_FILENAME_PREFIX + DateTime.now().toString("yyyy-mm-dd"), true);
	}
	
	/**
	 * By default transmissions are not deleted after decoding
	 * Subclasses may override this if transmissions should be deleted after decoding.
	 * 
	 * @param transmission
	 * @return
	 */
	public boolean deleteTransmissionUponDecoding(Transmission<?> transmission)
	{
		return false; // default
	}
	
	public abstract SMSClient getSMSClient();
	
	public abstract GeoKeyClient getGeoKeyClient();
	
	// ================= SEND =================
	
	public synchronized void sendRecords(Model model, Correspondent receiver)
	{
		List<Record> recsToSend = new ArrayList<Record>();
		
		// Query for unsent (as in, not associated with a transmission) records for the given receiver & model:
		CollectionUtils.addAllIgnoreNull(recsToSend, transmissionStore.retrieveTransmittableRecordsWithoutTransmission(receiver, model));
		
		//Also include transmittable records which have a transmission which was never sent or not received since the timeout:
		//CollectionUtils.addAllIgnoreNull(recsToSend, transmissionStore.retrieveTransmittableRecordsWithTimedOutTransmission(receiver, model, 5));
		// TODO what is a good timeout??
		
		addLogLine("Records to send: " + recsToSend.size());

		// Create RecordsPayloads & Transmissions (add as many records as possible to each):
		RecordsPayload payload = null;
		Iterator<Record> recsToSendIt = recsToSend.iterator();
		Record record = null;
		while(recsToSendIt.hasNext() || record != null || payload != null)
		{
			// Get next record if needed:
			if(record == null && recsToSendIt.hasNext())
				record = recsToSendIt.next();
			
			// Create new payload & transmission if needed:
			if(payload == null)
			{
				// Create a new Payload...
				payload = new RecordsPayload(receiver.favoursLosslessPayload());

				// ... and a new Transmission:
				createOutgoingTransmission(payload, receiver);
			}

			// Add record if we have one:
			if(record != null)
			{
				try
				{
					payload.addRecord(record); // may throw any of the 4 Exception types caught below
					
					// if we get here the record was added successfully...
					record = null; // make sure we use a new record in the next iteration
					continue; // go to next iteration to try to add more records
				}
				catch(IllegalArgumentException e) // may happen if record is not transmittable or not fully filled
				{	// skip record and move on:
					record = null;
					continue;
				}
				catch(TransmissionCapacityExceededException e) // when the payload/transmission is full
				{
					/* do nothing, current transmission will be sent below and current record
					 * will be added to a new payload/transmission in the next iteration.*/
				}
				catch(TransmissionSendingException | IllegalStateException e)
				{	// should never happen really
					transmissionClient.logError("Error upon preparing RecordsPayload", e);
					payload = null;
					continue;
				}
			}
			
			// Store & send the transmission:
			boolean sent = storeAndSend(payload.getTransmission());
			
			// Associate the transmittables with the transmission:
			if(sent)
				for(Record recBeingSent : payload.getRecords())
					transmissionStore.storeTransmittableRecord(receiver, recBeingSent.getReference(), payload.getTransmission());
			
			// Make payload null so a new was is created in the next iteration:
			payload = null;
		}
	}
	
	/**
	 * Send a {@link ModelQueryPayload} for the given {@link Model} to the given receiver.
	 * 
	 * @param model
	 * @param receiver
	 * @param the "modelQueryID" (to be used to call {@link #getModelQueryStatus(int)}), which is the localID of the transmission used to send the {@link ModelQueryPayload}
	 */
	public synchronized int sendModelQuery(Model model, Correspondent receiver)
	{
		// Create ModelQueryPayload...
		ModelQueryPayload payload = new ModelQueryPayload(model.id);

		// ... and a new Transmission:
		Transmission<?> transmission = createOutgoingTransmission(payload, receiver);
		
		// Store & send the transmission:
		storeAndSend(transmission);

		// Return local id:
		return transmission.getLocalID();
	}
	
	/**
	 * Returns the status of a previously sent ModelQuery.
	 * 
	 * @param modelQueryID as returned by {@link #sendModelQuery(Model, Correspondent)}
	 * @return a {@link ModelQueryStatus}
	 * @throws IllegalArgumentException
	 */
	public synchronized ModelQueryStatus getModelQueryStatus(int modelQueryID) throws IllegalArgumentException
	{
		Transmission<?> modelQueryT = transmissionStore.retrieveTransmission(false, modelQueryID);
		if(modelQueryT == null)
			throw new IllegalArgumentException("Cannot find a transmission for the given modelQueryID");
		if(!modelQueryT.hasResponse())
			return ModelQueryStatus.Pending; // no response yet...
		else
			return
				modelQueryT.getResponse().getPayloadType() == Payload.BuiltinType.Ack.ordinal() ?
					ModelQueryStatus.ModelPresent :
					ModelQueryStatus.ModelAbsent;
	}
	
	/**
	 * @param payload
	 * @param receiver
	 * @return
	 */
	protected Transmission<?> createOutgoingTransmission(Payload payload, Correspondent receiver)
	{
		switch(receiver.getTransmissionType())
		{
			case BINARY_SMS:
				return new BinarySMSTransmission(transmissionClient, (SMSCorrespondent) receiver, payload);
			case TEXTUAL_SMS:
				return new TextSMSTransmission(transmissionClient, (SMSCorrespondent) receiver, payload);
			case GeoKey:
				return new GeoKeyTransmission(transmissionClient, (GeoKeyAccount) receiver, payload);
			default:
				System.err.println("Unsupported transmission type: " + receiver.getTransmissionType());
				return null;
		}
	}
	
	/**
	 * @param transmission
	 * @return whether preparation, storing & sending were successful or not
	 */
	private boolean storeAndSend(Transmission<?> transmission)
	{
		try
		{
			// Prepare transmission for storage & sending:
			transmission.prepare();
			
			// Store "in-flight transmissions" to get local ID:
			transmissionStore.store(transmission); // update record now that it is prepared (payload hash has been computed, etc.)

			// Log sending attempt:
			addLogLine("OUTGOING TRANSMISSION", "Type: " +  transmission.getType().toString(), "SendingSideID: " + transmission.getLocalID(), "PAYLOAD: " + Payload.GetPayloadTypeString(transmission.getPayloadType()), "TO: " + transmission.getCorrespondent().toString());
			
			// actually send the transmission:
			transmission.send(this);
			
			// Success:
			return true;
		}
		catch(Exception e)
		{
			transmissionClient.logError("Error upon preparing/storing/sending transmission", e);
			return false;
		}
	}
	
	protected void storeAndSendResponse(ResponsePayload payload, Correspondent receiver)
	{
		Transmission<?> outgoing = createOutgoingTransmission(payload, receiver);
		
		// Store & send:
		if(storeAndSend(outgoing))
		{
			// Set subject response:
			payload.getSubject().setResponse(outgoing);
			try
			{
				transmissionStore.store(payload.getSubject());
			}
			catch(DBException e)
			{
				transmissionClient.logError("Error upon saving response subject");
			}
		}
	}
	
	// ================= RECEIVE =================

	/**
	 * Method that does most of the work for receiving Transmissions (if complete, read contents and act on them).
	 * 
	 * @param transmission the transmission that has been received, it is assumed to be complete!
	 * @throws TransmissionReceivingException when something goes wrong
	 */
	protected synchronized void doReceive(Transmission<?> transmission) throws TransmissionReceivingException
	{	
		addLogLine(	"INCOMING", "Transmission", transmission.getType().toString(),
					"From: " + transmission.getCorrespondent());

		// "Receive" the transmission (merge parts, decode, verify):
		transmission.receive(); // throws TransmissionReceivingException
		
		// Store/update transmission now that the payload type is known:
		try
		{
			transmissionStore.store(transmission);
		}
		catch(DBException dbE)
		{
			throw new TransmissionReceivingException(transmission, "Error upon storing/updating received transmission", dbE);
		}
		
		// Handle/receive the payload (also deals with PayloadDecodeExceptions):
		payloadReceiver.receive(transmission.getPayload()); // throws TransmissionReceivingException! (so no ACK will be sent if something goes wrong here)
			
		// Acknowledge reception if needed
		if(transmission.getPayload().acknowledgeReception())
			storeAndSendResponse(new AckPayload(transmission), transmission.getCorrespondent());
			
		// Delete transmission (and parts) from store if needed:
		if(deleteTransmissionUponDecoding(transmission))
			transmissionStore.deleteTransmission(transmission);
	}
	
	// ----- "handle"/"receive" methods for different transmission types:
	
	/**
	 * @param phoneNumber
	 * @param binarySMS
	 * @return
	 * @throws Exception
	 */
	public SMSCorrespondent getSendingCorrespondentFor(PhoneNumber phoneNumber, boolean binarySMS) throws Exception
	{
		// Try to find sender:
		SMSCorrespondent corr = transmissionStore.retrieveSMSCorrespondent(phoneNumber, binarySMS);
		if(corr == null)
			// make new correspondent
			corr = new SMSCorrespondent(Correspondent.UNKNOWN_SENDER_NAME, phoneNumber, binarySMS);
		return corr;
	}

	/**
	 * @param msg
	 */
	public synchronized void receiveSMS(Message<?, ?> msg) throws Exception
	{
		try
		{
			// Receive the message:
			smsReceiver.receive(msg);
			SMSTransmission<?> smsTrans = smsReceiver.transmission;
			
			// Store transmission:
			transmissionStore.store(smsTrans);
			
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
			addLogLine("ERROR", "Upon SMS message reception", ExceptionHelpers.getMessageAndCause(e));
			throw e;
		}
	}

	/**
	 * @param localID local ID of an incomplete SMSTransmission (i.e. the subject of the resend request)
	 * @param force if {@code true} the request is sent even if it is too early
	 * @return whether or not future resend requests may needed for this transmission 
	 * @throws Exception
	 */
	public synchronized void sendSMSResendRequest(int localID, boolean force)
	{
		// Query for the subject transmission:
		Transmission<?> trans = transmissionStore.retrieveTransmission(true, localID);
		
		// Check if it makes sense to send the request...
		if(trans == null || !( trans instanceof SMSTransmission) || trans.isComplete())
			return;

		// Cast to SMSTransmission:
		final SMSTransmission<?> smsTrans = (SMSTransmission<?>) trans;
		
		// Further checks...
		TimeStamp sendReqAt = smsTrans.getNextResendRequestSendingTime();
		if(sendReqAt == null || (!force && sendReqAt.isAfter(TimeStamp.now())))
			return; // either no more reqs are allowed, or it is too early to send the next one (shouldn't happen)
		
		addLogLine("PREPARING", "Outgoing resend request for incomplete transmission with local ID: " + localID);
		
		// Send request:
		storeAndSendResponse(new ResendRequestPayload(smsTrans, this), smsTrans.getCorrespondent());
	}
	
	/**
	 * Schedules resend requests for all incomplete SMSTransmissions
	 * To be called upon device boot.
	 * 
	 * @return whether any requests were scheduled
	 * @throws Exception
	 */
	public synchronized boolean scheduleSMSResendRequests() throws Exception
	{
		// Query for incomplete SMSTransmissions:
		List<SMSTransmission<?>> incompleteSMSTs = transmissionStore.retrieveIncompleteSMSTransmissions();		
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
		
		public void receive(Message<?, ?> smsMsg)
		{
			transmission = null; // wipe previous transmission !!!
			try
			{	// try finding an (incomplete) transmission this message belongs to (assuming this is not the first part):
				transmission = (SMSTransmission<?>) transmissionStore.retrieveTransmission(true, smsMsg.getTransmissionType(), smsMsg.getSender(), smsMsg.getSendingSideTransmissionID(), smsMsg.getPayloadHash(), smsMsg.getTotalParts());
			}
			catch(Exception e)
			{
				addLogLine("ERROR", "Upon querying for existing SMS transmission", ExceptionHelpers.getMessageAndCause(e));
			}
			
			// Handle specific message type:
			smsMsg.handle(this);
		}
		
		@Override
		public void handle(BinaryMessage binSms)
		{
			log(binSms, true);
			if(transmission == null)
				// we received the first part
				transmission = new BinarySMSTransmission(transmissionClient, binSms);
			else
				// this is not the first part, add it to the existing transmission:
				((BinarySMSTransmission) transmission).addPart(binSms);
		}

		@Override
		public void handle(TextMessage txtSms)
		{
			log(txtSms, false);
			if(transmission == null)
				// we received the first part
				transmission = new TextSMSTransmission(transmissionClient, txtSms);
			else
				// this is not the first part, add it to the existing transmission:
				((TextSMSTransmission) transmission).addPart(txtSms);
		}
		
		private void log(Message<?, ?> msg, boolean binary)
		{
			addLogLine(	"INCOMING", "SMS", binary ? "Binary" : "Text",
						"SendingSideTransmissionID: " + msg.getSendingSideTransmissionID(),
						"Part: " + msg.getPartNumber() + "/" + msg.getTotalParts(),
						"From: "+ msg.getSender());
		}
		
	}
	
	/**
	 * Helper class with "handle" methods for different payload types (called once the transmission is complete).
	 * 
	 * @author benelliott, mstevens
	 */
	public abstract class PayloadReceiver implements Payload.Handler
	{
		
		/**
		 * Receive/decode payload
		 * 
		 * @param payload
		 * @throws TransmissionReceivingException
		 */
		public void receive(Payload payload) throws TransmissionReceivingException
		{
			// Null check:
			if(payload == null)
				throw new TransmissionReceivingException(new NullPointerException("Payload is null!"));
				
			// Deal with possible PayloadDecodeException:
			if(payload.hasDecodeException())
			{
				PayloadDecodeException exception = payload.getDecodeException();
				
				// Deal with UnknownModelException cause:
				if(exception.getCause() instanceof UnknownModelException)
				{
					UnknownModelException ume = (UnknownModelException) exception.getCause();
					// Send model request payload:
					storeAndSendResponse(new ModelRequestPayload(payload.getTransmission(), ume.getModelID()), payload.getTransmission().getCorrespondent());
				}
				
				// re-throw:
				throw exception; // (so no ACK is sent!)
			}
			
			// Handle payload:
			try
			{
				payload.handle(this);
			}
			catch(Exception e)
			{
				if(e instanceof TransmissionReceivingException)
					throw (TransmissionReceivingException) e;
				else
					throw new TransmissionReceivingException(payload.getTransmission(), "Error upon handing payload (type: " + payload.getType() + ")", e);
			}
		}
		
		@Override
		public void handle(RecordsPayload recordsPayload) throws Exception
		{
			if(logger != null)
			{
				Map<Schema, List<Record>> recordsBySchema = recordsPayload.getRecordsBySchema();
				TransactionalStringBuilder bldr = new TransactionalStringBuilder(", ");
				for(Schema schema : recordsPayload.getSchemata())
				{
					bldr.openTransaction(" ");
					bldr.append(schema.getName());
					bldr.append("(");
					bldr.append("" + (recordsBySchema.get(schema) != null ? recordsBySchema.get(schema).size() : 0), false);
					bldr.append(")", false);
					bldr.commitTransaction();
				}
				logger.addLine("INCOMING RECORDS", "TOTAL: " + recordsPayload.getNumberOfRecords(), "SCHEMATA: " + bldr.toString());
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
		
		private Transmission<?> handleResponse(ResponsePayload response) throws Exception
		{
			Transmission<?> subject = transmissionStore.retrieveTransmission(false, response.getSubjectSenderSideID(), response.getSubjectPayloadHash());
			
			if(subject != null)
				subject.getSentCallback().onResponse(response);
			else
				transmissionClient.logError("No matching transmission (ID " + response.getSubjectSenderSideID() + "; payload hash: " + response.getSubjectPayloadHash() + " ) found in the database for acknowledgement from sender " + response.getTransmission().getCorrespondent());
			
			return subject;
		}
		
		@Override
		public void handle(AckPayload ack) throws Exception
		{
			// find the appropriate sent transmission (i.e. the subject of the ACK) and mark it as ACKed by setting the receivedAt time to ackPayload.getSubjectReceivedAt()
			Transmission<?> subject = handleResponse(ack);
			
			addLogLine(	"INCOMING", "Payload", "ACK",
						"Subject local ID: " + ack.getSubjectSenderSideID(),
						"Subject hash: " + ack.getSubjectPayloadHash(),
						"Subject found: " + (subject != null));
			if(subject != null)
			{
				Payload subjectPayload = subject.getPayload();
				if(subjectPayload != null)
					subjectPayload.handle(payloadAckHandler); 	// Payload-specific ACK handling
			}
		}
		
		@Override
		public void handle(ResendRequestPayload resendReq) throws Exception
		{
			// get the appropriate sent transmission (i.e. the subject of the request) - it will definitely be an SMSTransmission since resend requests are only concerned with SMS (at least for now)
			SMSTransmission<?> subject = (SMSTransmission<?>) handleResponse(resendReq);
			
			addLogLine(	"INCOMING", "Payload", "ResendReq",
						"Subject local ID: " + resendReq.getSubjectSenderSideID(),
						"Subject hash: " + resendReq.getSubjectPayloadHash(),
						"Subject total parts: " + resendReq.getSubjectTotalParts(),
						"Requested part numbers: " + StringUtils.join(resendReq.getRequestedPartNumbers(), ", "),
						"Subject found: " + (subject != null));
			if(subject != null) // subject is known ...
			{
				if(!subject.isReceived()) // ... and we haven't received a ACK yet (check just in case):
				{
					// Resend requested parts:
					subject.resend(TransmissionController.this, resendReq.getRequestedPartNumbers());
				}
			}			
		}
		
		@Override
		public void handle(ModelRequestPayload modelRequestPayload) throws Exception
		{
			handleResponse(modelRequestPayload);
			
			addLogLine("INCOMING MODEL REQUEST", "ID: " + modelRequestPayload.getUnknownModelID());
			
			// TODO somehow report that project is missing on receiver (tray alert?)
		}
		
		@Override
		public void handle(ModelQueryPayload modelQueryPayload) throws Exception
		{
			// everything is done already in #receive(Payload) (see above)
		}
		
	}
	
	private class DefaultPayloadReceiver extends PayloadReceiver
	{

		@Override
		public void handle(Payload customPayload, int type) throws Exception
		{
			addLogLine("INCOMING CUSTOM", "TRANSMISSION ID: " + customPayload.getTransmission().getLocalID());
			transmissionClient.logError("Receiving custom payload (type: " + type + ") not supported!");
		}
		
	}
	
	/**
	 * @author mstevens
	 *
	 */
	private class PayloadAckHandler implements Payload.Handler
	{
		
		@Override
		public void handle(RecordsPayload recordsPayload) throws Exception
		{
			// does nothing (for now)
			// Note: We could delete the transmittables here, but then we lose an easy way to query which records have been sent to which receiver(s)
		}

		@Override
		public void handle(AckPayload ackPayload) throws Exception
		{
			// never happens because AckPayload#acknowledgeReception() returns false.
		}

		@Override
		public void handle(ResendRequestPayload resendRequestPayload) throws Exception
		{
			// never happens because ResendRequestPayload#acknowledgeReception() returns false.
		}

		@Override
		public void handle(ModelQueryPayload modelQueryPayload) throws Exception
		{
			// TODO Auto-generated method stub
		}

		@Override
		public void handle(ModelRequestPayload modelRequestPayload) throws Exception
		{
			// TODO ?
		}

		@Override
		public void handle(Payload customPayload, int type) throws Exception
		{
			// TODO deal with custom payload
		}
		
	}
	
	protected abstract String getApplicationInfo();
	
	public void addLogLine(String... fields)
	{
		if(logger != null)
			logger.addLine(fields);
	}
	
	/**
	 * @return the transmissionStore
	 */
	public TransmissionStore getTransmissionStore()
	{
		return transmissionStore;
	}

	@Override
	public void finalize()
	{
		discard();
	}
	
	public void discard()
	{
		transmissionClient.recordStoreHandle.doneUsing(this);
		transmissionClient.transmissionStoreHandle.doneUsing(this);
	}
	
}
