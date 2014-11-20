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

package uk.ac.ucl.excites.sapelli.transmission.db;

import java.io.File;
import java.nio.charset.Charset;

import uk.ac.ucl.excites.sapelli.shared.db.Store;
import uk.ac.ucl.excites.sapelli.shared.db.StoreBackuper;
import uk.ac.ucl.excites.sapelli.shared.db.StoreClient;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStoreProvider;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ByteArrayColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.TimeStampColumn;
import uk.ac.ucl.excites.sapelli.storage.model.indexes.AutoIncrementingPrimaryKey;
import uk.ac.ucl.excites.sapelli.storage.queries.FirstRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Order;
import uk.ac.ucl.excites.sapelli.storage.queries.Source;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.RuleConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.RuleConstraint.Comparison;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.transmission.Payload;
import uk.ac.ucl.excites.sapelli.transmission.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.modes.http.HTTPTransmission;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.Message;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSAgent;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.binary.BinarySMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.text.TextSMSTransmission;

/**
 * Class to handle storage of transmissions and their parts. Based on {@link RecordStore}.
 * 
 * TODO delete methods
 * 
 * @author mstevens, Michalis Vitos
 */
public class TransmissionStore implements Store, StoreClient
{
	
	// STATICS---------------------------------------------
	static private final Charset UTF8_CHARSET = Charset.forName("UTF-8");
	
	// Transmission storage model:
	//	Model:
	static public final Model TRANSMISSION_MANAGEMENT_MODEL = new Model(TransmissionClient.TRANSMISSION_MANAGEMENT_MODEL_ID, "TransmissionManagement");
	// Schema(s) & columns:
	//	Transmission Schema
	static final public Schema TRANSMISSION_SCHEMA = new Schema(TRANSMISSION_MANAGEMENT_MODEL, "Transmission");
	static final public IntegerColumn TRANSMISSION_COLUMN_ID = new IntegerColumn("ID", false, Transmission.TRANSMISSION_ID_FIELD);
	static final public IntegerColumn TRANSMISSION_COLUMN_REMOTE_ID = new IntegerColumn("RemoteID", true, Transmission.TRANSMISSION_ID_FIELD);
	static final public IntegerColumn TRANSMISSION_COLUMN_TYPE = new IntegerColumn("Type", false, false, Integer.SIZE);
	static final public IntegerColumn TRANSMISSION_COLUMN_PAYLOAD_HASH = new IntegerColumn("PayloadHash", false, Transmission.PAYLOAD_HASH_FIELD);
	static final public IntegerColumn TRANSMISSION_COLUMN_PAYLOAD_TYPE = new IntegerColumn("PayloadType", true, Payload.PAYLOAD_TYPE_FIELD);
	static final public StringColumn TRANSMISSION_COLUMN_SENDER = StringColumn.ForCharacterCount("Sender", false, Transmission.CORRESPONDENT_MAX_LENGTH);
	static final public StringColumn TRANSMISSION_COLUMN_RECEIVER = StringColumn.ForCharacterCount("Receiver", false, Transmission.CORRESPONDENT_MAX_LENGTH);
	static final public IntegerColumn TRANSMISSION_COLUMN_NUMBER_OF_PARTS = new IntegerColumn("NumberOfParts", false, false, Integer.SIZE);
	//	Columns shared with TransmisionPart:
	static final public TimeStampColumn COLUMN_SENT_AT = TimeStampColumn.JavaMSTime("SentAt", true, false);
	static final public TimeStampColumn COLUMN_RECEIVED_AT = TimeStampColumn.JavaMSTime("ReceivedAt", true, false);
	//	Add columns and index to Transmission Schema & seal it:
	static
	{
		TRANSMISSION_SCHEMA.addColumn(TRANSMISSION_COLUMN_ID);
		TRANSMISSION_SCHEMA.addColumn(TRANSMISSION_COLUMN_REMOTE_ID);
		TRANSMISSION_SCHEMA.addColumn(TRANSMISSION_COLUMN_TYPE);
		TRANSMISSION_SCHEMA.addColumn(TRANSMISSION_COLUMN_PAYLOAD_HASH);
		TRANSMISSION_SCHEMA.addColumn(TRANSMISSION_COLUMN_PAYLOAD_TYPE);
		TRANSMISSION_SCHEMA.addColumn(TRANSMISSION_COLUMN_SENDER);
		TRANSMISSION_SCHEMA.addColumn(TRANSMISSION_COLUMN_RECEIVER);
		TRANSMISSION_SCHEMA.addColumn(TRANSMISSION_COLUMN_NUMBER_OF_PARTS);
		TRANSMISSION_SCHEMA.addColumn(COLUMN_SENT_AT);
		TRANSMISSION_SCHEMA.addColumn(COLUMN_RECEIVED_AT);
		TRANSMISSION_SCHEMA.setPrimaryKey(new AutoIncrementingPrimaryKey("IDIdx", TRANSMISSION_COLUMN_ID));
		TRANSMISSION_SCHEMA.seal();
	}
	//	Transmission Part Schema
	static final public Schema TRANSMISSION_PART_SCHEMA = new Schema(TRANSMISSION_MANAGEMENT_MODEL, "TransmissionPart");
	static final public ForeignKeyColumn TRANSMISSION_PART_COLUMN_TRANSMISSION_ID = new ForeignKeyColumn(TRANSMISSION_SCHEMA.getName() + TRANSMISSION_COLUMN_ID.getName(), TRANSMISSION_SCHEMA, false);
	static final public IntegerColumn TRANSMISSION_PART_COLUMN_NUMBER = new IntegerColumn("PartNumber", false, false, Integer.SIZE);
	static final public TimeStampColumn TRANSMISSION_PART_COLUMN_DELIVERED_AT = TimeStampColumn.JavaMSTime("DeliveredAt", true, false);
	static final public ByteArrayColumn TRANSMISSION_PART_COLUMN_BODY = new ByteArrayColumn("Body", false);
	static final public IntegerColumn TRANSMISSION_PART_COLUMN_BODY_BIT_LENGTH = new IntegerColumn("BodyBitLength", false, false, Integer.SIZE);
	static
	{	// Add columns to Transmission Part Schema & seal it:
		TRANSMISSION_PART_SCHEMA.addColumn(TRANSMISSION_PART_COLUMN_TRANSMISSION_ID);
		TRANSMISSION_PART_SCHEMA.addColumn(COLUMN_SENT_AT);
		TRANSMISSION_PART_SCHEMA.addColumn(TRANSMISSION_PART_COLUMN_DELIVERED_AT);
		TRANSMISSION_PART_SCHEMA.addColumn(COLUMN_RECEIVED_AT);
		TRANSMISSION_PART_SCHEMA.addColumn(TRANSMISSION_PART_COLUMN_BODY);
		TRANSMISSION_PART_SCHEMA.addColumn(TRANSMISSION_PART_COLUMN_BODY_BIT_LENGTH);
		TRANSMISSION_PART_SCHEMA.seal();
		// Seal the model:
		TRANSMISSION_MANAGEMENT_MODEL.seal();
	}
	
	// DYNAMICS--------------------------------------------
	private final TransmissionClient client;
	private final RecordStoreProvider recordStoreProvider;
	private final RecordStore recordStore;

	public TransmissionStore(TransmissionClient client, RecordStoreProvider recordStoreProvider) throws DBException
	{
		this.client = client;
		this.recordStoreProvider = recordStoreProvider;
		this.recordStore = recordStoreProvider.getRecordStore(this);
	}
	
	/**
	 * Creates a Record representing a Transmission.
	 * The values of all columns will be set except for Sender, Receiver & NumberOfParts.
	 * 
	 * @param transmission
	 * @return
	 */
	private Record createTransmissionRecord(Transmission transmission)
	{
		// Create a transmission record:
		Record tRec = TRANSMISSION_SCHEMA.createRecord();
		// Set values:
		if(transmission.isLocalIDSet())
			TRANSMISSION_COLUMN_ID.storeValue(tRec, transmission.getLocalID());
		TRANSMISSION_COLUMN_REMOTE_ID.storeValue(tRec, transmission.getRemoteID());
		TRANSMISSION_COLUMN_TYPE.storeValue(tRec, transmission.getType().ordinal());
		TRANSMISSION_COLUMN_PAYLOAD_HASH.storeValue(tRec, transmission.getPayloadHash()); // payload hash should always be set before storage
		if(transmission.isPayloadSet())
			TRANSMISSION_COLUMN_PAYLOAD_TYPE.storeValue(tRec, transmission.getPayload().getType());
		COLUMN_SENT_AT.storeValue(tRec, transmission.getSentAt());
		COLUMN_RECEIVED_AT.storeValue(tRec, transmission.getReceivedAt());
		// Return:
		return tRec;
	}
	
	/**
	 * Creates a Record representing an SMSTransmission.
	 * The values of all columns will be set.
	 * 
	 * @param transmission
	 * @return
	 */
	private Record createTransmissionRecord(SMSTransmission<?> transmission)
	{
		// Get record for transmission:
		Record tRec = createTransmissionRecord((Transmission) transmission);
		// Set remaining values:
		TRANSMISSION_COLUMN_NUMBER_OF_PARTS.storeValue(tRec, transmission.getTotalNumberOfParts());
		if(transmission.isSenderSet())
			TRANSMISSION_COLUMN_SENDER.storeValue(tRec, transmission.getSender().toString());
		if(transmission.isReceiverSet())
			TRANSMISSION_COLUMN_SENDER.storeValue(tRec, transmission.getReceiver().toString());
		// Return:
		return tRec;
	}
	
	/**
	 * @param transmission assumed to have all values set, except the (local) ID when inserting
	 * @throws Exception 
	 */
	private void doStoreTransmission(Transmission transmission, Record transmissionRecord) throws Exception
	{
		// Store the transmission
		recordStore.store(transmissionRecord);
		
		// Transmission ID should now be set in the record...
		if(transmission.isLocalIDSet()) // if the object already had a local transmissionID...
		{	// then it should match the ID on the record, so let's verify:
			if(transmission.getLocalID() != TRANSMISSION_COLUMN_ID.retrieveValue(transmissionRecord))
				throw new IllegalStateException("Non-matching transmission ID"); // this should never happen
		}
		else
			// Set local transmissionID in object as on the record: 
			transmission.setLocalID(TRANSMISSION_COLUMN_ID.retrieveValue(transmissionRecord).intValue());
	}
	
	public void storeTransmission(SMSTransmission<?> smsTransmission) throws Exception
	{
		// Start transaction
		recordStore.startTransaction();
		
		try
		{
			// Create & store record:
			Record tRec = createTransmissionRecord(smsTransmission);
			doStoreTransmission(smsTransmission, tRec); // after this the localID should always be known
			
			// Parts...
			for(Message msg : smsTransmission.getParts())
			{
				Record tPartRec = TRANSMISSION_PART_SCHEMA.createRecord();
				TRANSMISSION_PART_COLUMN_TRANSMISSION_ID.storeValue(tPartRec, tRec.getReference()); // set foreign key
				TRANSMISSION_PART_COLUMN_NUMBER.storeValue(tPartRec, msg.getPartNumber());
				msg.setBody(this, tPartRec);
				COLUMN_SENT_AT.storeValue(tPartRec, msg.getSentAt());
				TRANSMISSION_PART_COLUMN_DELIVERED_AT.storeValue(tPartRec, msg.getDeliveredAt());
				COLUMN_RECEIVED_AT.storeValue(tPartRec, msg.getReceivedAt());
				
				// Store part record:
				recordStore.store(tPartRec);
			}
		}
		catch(Exception e)
		{
			recordStore.rollbackTransactions();
			throw e;
		}
		
		// Commit transaction
		recordStore.commitTransaction();
	}
	
	public void setPartBody(BitArray bodyBits, Record transmissionPartRecord)
	{
		TRANSMISSION_PART_COLUMN_BODY.storeValue(transmissionPartRecord, bodyBits.toByteArray());
		TRANSMISSION_PART_COLUMN_BODY_BIT_LENGTH.storeValue(transmissionPartRecord, bodyBits.length());
	}
	
	public void setPartBody(String bodyString, Record transmissionPartRecord)
	{
		byte[] bytes = bodyString.getBytes(UTF8_CHARSET);
		TRANSMISSION_PART_COLUMN_BODY.storeValue(transmissionPartRecord, bytes);
		TRANSMISSION_PART_COLUMN_BODY_BIT_LENGTH.storeValue(transmissionPartRecord, bytes.length * Byte.SIZE);
	}
	
	public void storeTransmission(HTTPTransmission httpTransmission) throws Exception
	{
		// Start transaction
		recordStore.startTransaction();
		
		try
		{
			// Create record:
			Record tRec = createTransmissionRecord(httpTransmission);
			
			// Set receiver (= serverURL) and number of parts (always = 1):
			TRANSMISSION_COLUMN_RECEIVER.storeValue(tRec, httpTransmission.getServerURL());
			TRANSMISSION_COLUMN_NUMBER_OF_PARTS.storeValue(tRec, 1);
			
			// Store the transmission record:
			doStoreTransmission(httpTransmission, tRec); // after this the localID should always be known
			
			// Create a single transmission part (only used to store the body):
			Record tPartRec = TRANSMISSION_PART_SCHEMA.createRecord();
			TRANSMISSION_PART_COLUMN_TRANSMISSION_ID.storeValue(tPartRec, tRec.getReference()); // set foreign key
			TRANSMISSION_PART_COLUMN_NUMBER.storeValue(tPartRec, 1);
			byte[] bytes = httpTransmission.getBody();
			TRANSMISSION_PART_COLUMN_BODY.storeValue(tPartRec, bytes);
			TRANSMISSION_PART_COLUMN_BODY_BIT_LENGTH.storeValue(tPartRec, bytes.length * Byte.SIZE);
			
			// Store the part:
			recordStore.store(tPartRec);
		}
		catch(Exception e)
		{
			recordStore.rollbackTransactions();
			throw e;
		}
		
		// Commit transaction
		recordStore.commitTransaction();
	}
	
	/**
	 * @param localID
	 * @return the Transmission with the given {@code localID}, or {@code null} if no such transmission was found.
	 */
	public Transmission retrieveTransmission(int localID)
	{
		// Query for record:
		Record tRec = recordStore.retrieveRecord(new FirstRecordQuery(Source.From(TRANSMISSION_SCHEMA), Order.UNDEFINED, new RuleConstraint(TRANSMISSION_COLUMN_ID, Comparison.EQUAL, Long.valueOf(localID))));
		
		// Null check:
		if(tRec == null)
			return null; // no such transmission found
		
		// Values:
		Transmission.Type type = Transmission.Type.values()[TRANSMISSION_COLUMN_TYPE.retrieveValue(tRec).intValue()]; 
		Integer remoteID = TRANSMISSION_COLUMN_REMOTE_ID.isValueSet(tRec) ? TRANSMISSION_COLUMN_REMOTE_ID.retrieveValue(tRec).intValue() : null; 
		int payloadHash = TRANSMISSION_COLUMN_PAYLOAD_HASH.retrieveValue(tRec).intValue();
		String sender = TRANSMISSION_COLUMN_SENDER.retrieveValue(tRec);
		String receiver = TRANSMISSION_COLUMN_RECEIVER.retrieveValue(tRec);
		TimeStamp sentAt = COLUMN_SENT_AT.retrieveValue(tRec);
		TimeStamp receivedAt = COLUMN_RECEIVED_AT.retrieveValue(tRec);
		
		// Query for part records:
		//TODO
		
		//List<Record> tPartRecs = recordStore.retrieveRecords(new RecordsQuery(TRANSMISSION_PART_SCHEMA, new RuleConstraint(TRANSMISSION_PART_COLUMN_TRANSMISSION_ID, 
		
		// Construct object:
		switch(type)
		{
			case BINARY_SMS:
			case TEXTUAL_SMS:
				return createSMSTransmission(type, localID, remoteID, payloadHash, sender, receiver, sentAt, receivedAt);
			case HTTP:
				
			default:
				throw new IllegalStateException("Unsupported transmission type");
			
		}
	}
	
	private SMSTransmission<?> createSMSTransmission(Transmission.Type type, int localID, Integer remoteID, int payloadHash, String sender, String receiver, TimeStamp sentAt, TimeStamp receivedAt)
	{
		//TODO
		return null;
		
	}
	
	public BinarySMSTransmission retrieveBinarySMSTransmission(SMSAgent correspondent, boolean sent, int payloadHash)
	{
		//TODO
		// throw special exception when not unique		
		return null;
	}
	
	public TextSMSTransmission retrieveTextSMSTransmission(SMSAgent correspondent, boolean sent, int payloadType, int payloadHash)
	{
		//TODO
		return null;
	}

	public HTTPTransmission retrieveHTTPTransmission(int payloadType, int payloadHash)
	{
		//TODO
		return null;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.shared.db.Store#finalise()
	 */
	@Override
	public void finalise() throws DBException
	{
		recordStoreProvider.discardStoreUsage(recordStore, this); // signal to recordStoreProvider that this StoreClient is no longer using the recordStore
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.shared.db.Store#backup(uk.ac.ucl.excites.sapelli.shared.db.StoreBackuper, java.io.File)
	 */
	@Override
	public void backup(StoreBackuper backuper, File destinationFolder) throws DBException
	{
		backuper.addStoreForBackup(recordStore);
	}
	
}
