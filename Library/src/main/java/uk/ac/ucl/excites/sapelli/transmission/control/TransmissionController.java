/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
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
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.joda.time.DateTime;

import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.io.FileStorageException;
import uk.ac.ucl.excites.sapelli.shared.util.Logger;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.shared.util.TransactionalStringBuilder;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;
import uk.ac.ucl.excites.sapelli.transmission.model.Payload;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.model.content.AckPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.ModelAccessUnauthorisedPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.ModelQueryPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.ModelRequestPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.NoSuchTransmissionPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.RecordsPayload;
import uk.ac.ucl.excites.sapelli.transmission.model.content.ResponsePayload;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey.GeoKeyServer;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey.GeoKeyTransmission;
import uk.ac.ucl.excites.sapelli.transmission.protocol.geokey.GeoKeyClient;
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
		ModelAbsent,
		ModelAccessUnauthorised
	};

	// Client:
	private TransmissionClient transmissionClient;
	
	// Stores:
	protected final RecordStore recordStore;
	protected final TransmissionStore transmissionStore;
	
	// Handlers:
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
		return new Logger(logsFolder.getAbsolutePath(), LOG_FILENAME_PREFIX + DateTime.now().toString("yyyy-MM-dd"), true);
	}
	
	/**
	 * By default transmissions are not deleted after decoding
	 * Subclasses may override this if transmissions should be deleted after decoding.
	 * 
	 * @param transmission
	 * @return
	 */
	public boolean deleteTransmissionUponReception(Transmission<?> transmission)
	{
		return false; // default
	}
	
	public abstract GeoKeyClient getGeoKeyClient();
	
	// ================= SEND =================
	
	public synchronized void sendRecords(Model model, Correspondent receiver)
	{
		// Retrieve records of given model that need to be sent to given receiver:
		List<Record> recsToSend = transmissionStore.retrieveRecordsToTransmitNow(receiver, model);
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
		{
			Payload.BuiltinType responseType = Payload.getBuiltinType(modelQueryT.getResponse().getPayloadType());
			if(responseType == null)
				return ModelQueryStatus.Pending;
			switch(responseType)
			{
				case Ack :
					return ModelQueryStatus.ModelPresent;
				case ModelAccessUnauthorised :
					return ModelQueryStatus.ModelAccessUnauthorised;
				case ModelRequest :
					return ModelQueryStatus.ModelAbsent;
				default:
					return ModelQueryStatus.Pending; 
			}
		}	
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
			case GeoKey:
				return new GeoKeyTransmission(transmissionClient, (GeoKeyServer) receiver, payload);
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
	
	protected void storeAndSendResponse(ResponsePayload payload)
	{
		Transmission<?> outgoing = createOutgoingTransmission(payload, payload.getSubject().getCorrespondent());
		
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
			storeAndSendResponse(new AckPayload(transmission));
			
		// Delete transmission (and parts) from store if needed:
		if(deleteTransmissionUponReception(transmission))
			transmissionStore.deleteTransmission(transmission, true); // (delete by hiding)
	}
	
	// ----- "handle"/"receive" methods for different transmission types:
	
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
					storeAndSendResponse(new ModelRequestPayload(payload.getTransmission(), ume.getModelID()));
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
		
		private void handleResponse(ResponsePayload response, ResponseSubjectHandler subjectHandler) throws Exception
		{
			Transmission<?> subject = transmissionStore.retrieveTransmission(false, response.getSubjectSenderSideID(), response.getSubjectPayloadHash());
			
			// Announce (to log):
			subjectHandler.announce(subject);
			
			if(subject != null)
			{
				// Register response:
				subject.getSentCallback().onResponse(response);
				
				// Handle subject:
				subjectHandler.handle(subject);
			}
			else
			{
				transmissionClient.logError("No matching transmission (ID " + response.getSubjectSenderSideID() + "; payload hash: " + response.getSubjectPayloadHash() + ") found in the database for acknowledgement from sender " + response.getTransmission().getCorrespondent());
				// Reply with NoSuchTransmissionPayload:
				storeAndSendResponse(new NoSuchTransmissionPayload(response));
			}
		}
		
		@Override
		public void handle(final AckPayload ack) throws Exception
		{
			// find the appropriate sent transmission (i.e. the subject of the ACK) and mark it as ACKed by setting the receivedAt time to ackPayload.getSubjectReceivedAt()
			handleResponse(ack, new ResponseSubjectHandler()
			{
				@Override
				public void announce(Transmission<?> subject)
				{
					addLogLine(	"INCOMING", "Payload", "ACK",
							"Subject local ID: " + ack.getSubjectSenderSideID(),
							"Subject hash: " + ack.getSubjectPayloadHash(),
							"Subject found: " + (subject != null));
				}
				
				@Override
				public void handle(Transmission<?> subject) throws Exception
				{
					Payload subjectPayload = subject.getPayload();
					if(subjectPayload != null)
						subjectPayload.handle(payloadAckHandler); 	// Payload-specific ACK handling
				}
			});
		}
		
		@Override
		public void handle(final ModelRequestPayload modelRequestPayload) throws Exception
		{
			handleResponse(modelRequestPayload, new ResponseSubjectHandler()
			{
				@Override
				public void announce(Transmission<?> responseSubject)
				{
					addLogLine("INCOMING", "Payload", "ModelRequest", "ModelID: " + modelRequestPayload.getUnknownModelID());
				}
				
				@Override
				public void handle(Transmission<?> responseSubject) throws Exception
				{
					// TODO somehow report that project is missing on receiver (tray alert?)	
				}
			});
		}
		
		@Override
		public void handle(final ModelAccessUnauthorisedPayload modelAccessUnauthorisedPayload) throws Exception
		{
			handleResponse(modelAccessUnauthorisedPayload, new ResponseSubjectHandler()
			{
				@Override
				public void announce(Transmission<?> responseSubject)
				{
					addLogLine("INCOMING", "Payload", "ModelAccessUnauthorised");				
				}
				
				@Override
				public void handle(Transmission<?> responseSubject) throws Exception
				{
					// TODO show toast or tray alert on android?
				}
			});
		}
		
		@Override
		public void handle(ModelQueryPayload modelQueryPayload) throws Exception
		{
			// everything is done already in #receive(Payload) (see above)
		}

		/* (non-Javadoc)
		 * @see uk.ac.ucl.excites.sapelli.transmission.model.Payload.Handler#handle(uk.ac.ucl.excites.sapelli.transmission.model.content.NoSuchTransmissionPayload)
		 */
		@Override
		public void handle(NoSuchTransmissionPayload noSuchTransmissionPayload) throws Exception
		{
		}
		
	}
	
	private interface ResponseSubjectHandler
	{
		
		/**
		 * @param responseSubject - may be null
		 */
		public void announce(Transmission<?> responseSubject);
		
		/**
		 * @param responseSubject - never null
		 */
		public void handle(Transmission<?> responseSubject) throws Exception;
		
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
		public void handle(ModelQueryPayload modelQueryPayload) throws Exception
		{
			// nothing
		}

		@Override
		public void handle(ModelRequestPayload modelRequestPayload) throws Exception
		{
			// never happens because ModelRequestPayload#acknowledgeReception() returns false.
		}
		
		@Override
		public void handle(ModelAccessUnauthorisedPayload modelAccessUnauthorisedPayload) throws Exception
		{
			// never happens because ModelAccessUnauthorisedPayload#acknowledgeReception() returns false.
		}
		
		@Override
		public void handle(NoSuchTransmissionPayload noSuchTransmissionPayload) throws Exception
		{
			// never happens because NoSuchTransmissionPayload#acknowledgeReception() returns false.
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
