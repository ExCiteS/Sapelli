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
import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.db.Store;
import uk.ac.ucl.excites.sapelli.shared.db.StoreBackupper;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.RecordReference;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ByteArrayColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.storage.model.indexes.AutoIncrementingPrimaryKey;
import uk.ac.ucl.excites.sapelli.storage.queries.FirstRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Order;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.RuleConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.RuleConstraint.Comparison;
import uk.ac.ucl.excites.sapelli.storage.queries.sources.Source;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStampColumn;
import uk.ac.ucl.excites.sapelli.transmission.Payload;
import uk.ac.ucl.excites.sapelli.transmission.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.modes.http.HTTPTransmission;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.Message;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSAgent;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.SMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.binary.BinarySMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.text.TextMessage;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.text.TextSMSTransmission;

/**
 * Class to handle storage of transmissions and their parts. Based on {@link RecordStore}.
 * 
 * TODO delete methods
 * 
 * @author mstevens, Michalis Vitos
 */
public abstract class TransmissionStore extends Store implements StoreHandle.StoreUser
{
	
	// STATICS---------------------------------------------
	static private final Charset UTF8_CHARSET = Charset.forName("UTF-8");
	
	// Transmission storage model:
	//	Model:
	static public final Model TRANSMISSION_MANAGEMENT_MODEL = new Model(TransmissionClient.TRANSMISSION_MANAGEMENT_MODEL_ID, "TransmissionManagement", TransmissionClient.SCHEMA_FLAGS_TRANSMISSION_INTERNAL);
	// Schema(s) & columns:
	//	Transmission Schema
	static final public Schema TRANSMISSION_SCHEMA = TransmissionClient.CreateSchemaWithSuffixedTableName(TRANSMISSION_MANAGEMENT_MODEL, Transmission.class.getSimpleName(), "s");
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
	static final public Schema TRANSMISSION_PART_SCHEMA = TransmissionClient.CreateSchemaWithSuffixedTableName(TRANSMISSION_MANAGEMENT_MODEL, Transmission.class.getSimpleName() + "Part", "s");
	static final public ForeignKeyColumn TRANSMISSION_PART_COLUMN_TRANSMISSION_ID = new ForeignKeyColumn(TRANSMISSION_SCHEMA, false);
	static final public IntegerColumn TRANSMISSION_PART_COLUMN_NUMBER = new IntegerColumn("PartNumber", false, false, Integer.SIZE);
	static final public TimeStampColumn TRANSMISSION_PART_COLUMN_DELIVERED_AT = TimeStampColumn.JavaMSTime("DeliveredAt", true, false);
	static final public ByteArrayColumn TRANSMISSION_PART_COLUMN_BODY = new ByteArrayColumn("Body", false);
	static final public IntegerColumn TRANSMISSION_PART_COLUMN_BODY_BIT_LENGTH = new IntegerColumn("BodyBitLength", false, false, Integer.SIZE);
	static
	{	// Add columns to Transmission Part Schema & seal it:
		TRANSMISSION_PART_SCHEMA.addColumn(TRANSMISSION_PART_COLUMN_TRANSMISSION_ID);
		TRANSMISSION_PART_SCHEMA.addColumn(TRANSMISSION_PART_COLUMN_NUMBER);
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
	private final RecordStore recordStore;

	/**
	 * @param client
	 * @throws DBException
	 */
	public TransmissionStore(TransmissionClient client) throws DBException
	{
		this.client = client;
		this.recordStore = client.recordStoreHandle.getStore(this);
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
	public Transmission retrieveTransmissionForID(int localID) throws Exception
	{
		// Query for record:
		return retrieveTransmissionForQuery(TRANSMISSION_SCHEMA.createRecordReference(localID).getRecordQuery());
	}
	
	private Transmission retrieveTransmissionForQuery(SingleRecordQuery recordQuery)
	{
		// Query for record:
		Record tRec = recordStore.retrieveRecord(recordQuery);
				
		// Null check:
		if(tRec == null)
			return null; // no such transmission found
		
		// Values:
		Integer localID = TRANSMISSION_COLUMN_ID.retrieveValue(tRec).intValue();
		Transmission.Type type = Transmission.Type.values()[TRANSMISSION_COLUMN_TYPE.retrieveValue(tRec).intValue()]; 
		Integer remoteID = TRANSMISSION_COLUMN_REMOTE_ID.isValuePresent(tRec) ? TRANSMISSION_COLUMN_REMOTE_ID.retrieveValue(tRec).intValue() : null; 
		int payloadHash = TRANSMISSION_COLUMN_PAYLOAD_HASH.retrieveValue(tRec).intValue();
		String sender = TRANSMISSION_COLUMN_SENDER.retrieveValue(tRec);
		String receiver = TRANSMISSION_COLUMN_RECEIVER.retrieveValue(tRec);
		TimeStamp sentAt = COLUMN_SENT_AT.retrieveValue(tRec);
		TimeStamp receivedAt = COLUMN_RECEIVED_AT.retrieveValue(tRec);
		int totalParts = TRANSMISSION_COLUMN_NUMBER_OF_PARTS.retrieveValue(tRec).intValue();
		// Query for part records:		
		List<Record> tPartRecs = recordStore.retrieveRecords(new RecordsQuery(Source.From(TRANSMISSION_PART_SCHEMA), Order.AscendingBy(TRANSMISSION_PART_COLUMN_NUMBER), tRec.getRecordQueryConstraint()));
		// Construct object:
		SMSAgent senderAgent = SMSAgent.Parse(sender);
		SMSAgent receiverAgent = SMSAgent.Parse(receiver);
		switch(type)
		{
		case BINARY_SMS:
			// create a new SMSTransmission object:
			BinarySMSTransmission binarySMS =  new BinarySMSTransmission(client, localID, remoteID, payloadHash, sentAt, receivedAt, senderAgent, receiverAgent);
			// add each part we got from the query:
			for(Record partRecord : tPartRecs)
				binarySMS.receivePart(new BinaryMessage(binarySMS, TRANSMISSION_PART_COLUMN_NUMBER.retrieveValue(partRecord).intValue(), totalParts, sentAt, TRANSMISSION_PART_COLUMN_DELIVERED_AT.retrieveValue(partRecord), receivedAt, BitArray.FromBytes(TRANSMISSION_PART_COLUMN_BODY.retrieveValue(partRecord))));
			return binarySMS;
		case TEXTUAL_SMS:
			// create a new SMSTransmission object:
			TextSMSTransmission textSMS = new TextSMSTransmission(client, localID, remoteID, payloadHash, sentAt, receivedAt, senderAgent, receiverAgent);
			// add each part we got from the query:
			for(Record partRecord : tPartRecs)
				textSMS.receivePart(new TextMessage(textSMS, TRANSMISSION_PART_COLUMN_NUMBER.retrieveValue(partRecord).intValue(), totalParts, sentAt, TRANSMISSION_PART_COLUMN_DELIVERED_AT.retrieveValue(partRecord), receivedAt, new String(TRANSMISSION_PART_COLUMN_BODY.retrieveValue(partRecord))));
			return textSMS;
		case HTTP:
			return new HTTPTransmission(client, localID, remoteID, payloadHash, sentAt, receivedAt, receiver, TRANSMISSION_PART_COLUMN_BODY.retrieveValue(tPartRecs.get(0)) /* only one part for HTTP */ );
		default:
			throw new IllegalStateException("Unsupported transmission type");
		}
	}
	
	/**
	 * @param correspondent - the agent involved in the message
	 * @param sent - whether or not the correspondent is the sender of this message
	 * @param remoteID - the remote agent's ID for this transmission
	 * @param payloadHash - the hash of the transmission payload
	 * @return the (first) binary SMS transmission that obeys the conditions specified by the provided arguments.
	 */
	public BinarySMSTransmission retrieveBinarySMSTransmission(SMSAgent correspondent, boolean sent, int remoteID, int payloadHash)
	{
		return (BinarySMSTransmission) retrieveTransmissionForQuery(new FirstRecordQuery(TRANSMISSION_SCHEMA,
				new RuleConstraint(TRANSMISSION_COLUMN_TYPE, Comparison.EQUAL, Transmission.Type.BINARY_SMS.ordinal()),
				new RuleConstraint(sent ? TRANSMISSION_COLUMN_RECEIVER : TRANSMISSION_COLUMN_SENDER, Comparison.EQUAL, correspondent),
				new RuleConstraint(TRANSMISSION_COLUMN_REMOTE_ID, Comparison.EQUAL, remoteID),
				new RuleConstraint(TRANSMISSION_COLUMN_PAYLOAD_HASH, Comparison.EQUAL, payloadHash)));
	}
	
	/**
	 * @param correspondent - the agent involved in the message
	 * @param sent - whether or not the correspondent is the sender of this message
	 * @param remoteID - the remote agent's ID for this transmission
	 * @param payloadHash - the hash of the transmission payload
	 * @return the (first) textual SMS transmission that obeys the conditions specified by the provided arguments.
	 */
	public TextSMSTransmission retrieveTextSMSTransmission(SMSAgent correspondent, boolean sent, int remoteID, int payloadHash)
	{ 
		return (TextSMSTransmission) retrieveTransmissionForQuery(new FirstRecordQuery(TRANSMISSION_SCHEMA,
				new RuleConstraint(TRANSMISSION_COLUMN_TYPE, Comparison.EQUAL, Transmission.Type.TEXTUAL_SMS.ordinal()),
				new RuleConstraint(sent ? TRANSMISSION_COLUMN_RECEIVER : TRANSMISSION_COLUMN_SENDER, Comparison.EQUAL, correspondent),
				new RuleConstraint(TRANSMISSION_COLUMN_REMOTE_ID, Comparison.EQUAL, remoteID),
				new RuleConstraint(TRANSMISSION_COLUMN_PAYLOAD_HASH, Comparison.EQUAL, payloadHash)));
	}

	/**
	 * @param payloadType - the type of the payload
	 * @param payloadHash - the hash of the payload
	 * @return the (first) HTTP transmission that obeys the conditions specified by the provided arguments.
	 */
	public HTTPTransmission retrieveHTTPTransmission(int payloadType, int payloadHash)
	{
		return (HTTPTransmission) retrieveTransmissionForQuery(new FirstRecordQuery(TRANSMISSION_SCHEMA,
				new RuleConstraint(TRANSMISSION_COLUMN_TYPE, Comparison.EQUAL, Transmission.Type.HTTP.ordinal()),
				new RuleConstraint(TRANSMISSION_COLUMN_PAYLOAD_TYPE, Comparison.EQUAL, payloadType),
				new RuleConstraint(TRANSMISSION_COLUMN_PAYLOAD_HASH, Comparison.EQUAL, payloadHash)));
	}

	public void deleteTransmission(Transmission transmission)
	{
		if(!transmission.isLocalIDSet())
			return; // the transmission was never stored
		try
		{
			recordStore.startTransaction();
			
			// Get record reference:
			RecordReference tRecRef = TRANSMISSION_SCHEMA.createRecordReference(transmission.getLocalID());
				
			// Delete transmission part records:
			recordStore.delete(new RecordsQuery(Source.From(TRANSMISSION_PART_SCHEMA), tRecRef.getRecordQueryConstraint()));
			
			// Delete transmission record:
			recordStore.delete(tRecRef);
			
			recordStore.commitTransaction();
		}
		catch(Exception e)
		{
			try
			{
				recordStore.rollbackTransactions();
			}
			catch(Exception ignore) {}
		}
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.shared.db.Store#finalise()
	 */
	@Override
	protected void doClose() throws DBException
	{
		client.recordStoreHandle.doneUsing(this); // signal to recordStoreProvider that this StoreClient is no longer using the recordStore
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.shared.db.Store#backup(uk.ac.ucl.excites.sapelli.shared.db.StoreBackuper, java.io.File)
	 */
	@Override
	public void backup(StoreBackupper backuper, File destinationFolder) throws DBException
	{
		backuper.addStoreForBackup(recordStore);
	}
	
}
