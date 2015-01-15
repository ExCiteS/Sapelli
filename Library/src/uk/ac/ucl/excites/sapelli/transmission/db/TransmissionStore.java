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
import java.util.ArrayList;
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
import uk.ac.ucl.excites.sapelli.storage.model.columns.TimeStampColumn;
import uk.ac.ucl.excites.sapelli.storage.model.indexes.AutoIncrementingPrimaryKey;
import uk.ac.ucl.excites.sapelli.storage.queries.FirstRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Order;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery;
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
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.binary.BinarySMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.text.TextMessage;
import uk.ac.ucl.excites.sapelli.transmission.modes.sms.text.TextSMSTransmission;

/**
 * Class to handle storage of transmissions and their parts. Based on {@link RecordStore}.
 * 
 * @author mstevens, Michalis Vitos, 
 */
public abstract class TransmissionStore extends Store implements StoreHandle.StoreUser
{
	
	// STATICS---------------------------------------------
	static private final Charset UTF8_CHARSET = Charset.forName("UTF-8");
	
	static private byte[] StringToBytes(String str)
	{
		return str.getBytes(UTF8_CHARSET);
	}
	
	static private String BytesToString(byte[] bytes)
	{
		return new String(bytes, UTF8_CHARSET);
	}
	
	// Transmission storage model:
	//	Model:
	static public final Model TRANSMISSION_MANAGEMENT_MODEL = new Model(TransmissionClient.TRANSMISSION_MANAGEMENT_MODEL_ID, "TransmissionManagement");
	// Schemas & columns:
	//	Transmission cchemas:
	static final public Schema SENT_TRANSMISSION_SCHEMA = new Schema(TRANSMISSION_MANAGEMENT_MODEL, "SentTransmission");
	static final public Schema RECEIVED_TRANSMISSION_SCHEMA = new Schema(TRANSMISSION_MANAGEMENT_MODEL, "ReceivedTransmission");
	//	Transmission columns:
	static final public IntegerColumn TRANSMISSION_COLUMN_ID = new IntegerColumn("ID", false, Transmission.TRANSMISSION_ID_FIELD);
	static final public IntegerColumn TRANSMISSION_COLUMN_REMOTE_ID = new IntegerColumn("RemoteID", true, Transmission.TRANSMISSION_ID_FIELD);
	static final public IntegerColumn TRANSMISSION_COLUMN_TYPE = new IntegerColumn("Type", false);
	static final public IntegerColumn TRANSMISSION_COLUMN_PAYLOAD_HASH = new IntegerColumn("PayloadHash", false, Transmission.PAYLOAD_HASH_FIELD);
	static final public IntegerColumn TRANSMISSION_COLUMN_PAYLOAD_TYPE = new IntegerColumn("PayloadType", true, Payload.PAYLOAD_TYPE_FIELD);
	static final public StringColumn SENT_TRANSMISSION_COLUMN_RECEIVER = StringColumn.ForCharacterCount("Receiver", false, Transmission.CORRESPONDENT_MAX_LENGTH);
	static final public StringColumn RECEIVED_TRANSMISSION_COLUMN_SENDER = StringColumn.ForCharacterCount("Sender", false, Transmission.CORRESPONDENT_MAX_LENGTH);
	static final public IntegerColumn TRANSMISSION_COLUMN_NUMBER_OF_PARTS = new IntegerColumn("NumberOfParts", false, false, Integer.SIZE);
	//	Columns shared with TransmisionPart:
	static final public TimeStampColumn COLUMN_SENT_AT = TimeStampColumn.JavaMSTime("SentAt", true, false);
	static final public TimeStampColumn COLUMN_RECEIVED_AT = TimeStampColumn.JavaMSTime("ReceivedAt", true, false);
	//	Add columns and index to Transmission schemas & seal them:
	static
	{
		for(Schema schema : new Schema[] { SENT_TRANSMISSION_SCHEMA, RECEIVED_TRANSMISSION_SCHEMA } )
		{
			schema.addColumn(TRANSMISSION_COLUMN_ID);
			schema.addColumn(TRANSMISSION_COLUMN_REMOTE_ID);
			schema.addColumn(TRANSMISSION_COLUMN_TYPE);
			schema.addColumn(TRANSMISSION_COLUMN_PAYLOAD_HASH);
			schema.addColumn(TRANSMISSION_COLUMN_PAYLOAD_TYPE);
			if(schema == SENT_TRANSMISSION_SCHEMA)
				schema.addColumn(RECEIVED_TRANSMISSION_COLUMN_SENDER);
			else
				schema.addColumn(SENT_TRANSMISSION_COLUMN_RECEIVER);
			schema.addColumn(TRANSMISSION_COLUMN_NUMBER_OF_PARTS);
			schema.addColumn(COLUMN_SENT_AT);
			schema.addColumn(COLUMN_RECEIVED_AT);
			schema.setPrimaryKey(new AutoIncrementingPrimaryKey("IDIdx", TRANSMISSION_COLUMN_ID));
			schema.seal();
		}
	}
	//	Transmission Part schemas:
	static final public Schema SENT_TRANSMISSION_PART_SCHEMA = new Schema(TRANSMISSION_MANAGEMENT_MODEL, "SentTransmissionPart");
	static final public Schema RECEIVED_TRANSMISSION_PART_SCHEMA = new Schema(TRANSMISSION_MANAGEMENT_MODEL, "RecieverTransmissionPart");
	//	Transmission Part columns:
	static final public ForeignKeyColumn TRANSMISSION_PART_COLUMN_SENT_TRANSMISSION = new ForeignKeyColumn(SENT_TRANSMISSION_SCHEMA, false);
	static final public ForeignKeyColumn TRANSMISSION_PART_COLUMN_RECEIVED_TRANSMISSION = new ForeignKeyColumn(RECEIVED_TRANSMISSION_SCHEMA, false);
	static final public IntegerColumn TRANSMISSION_PART_COLUMN_NUMBER = new IntegerColumn("PartNumber", false, false, Integer.SIZE);
	static final public TimeStampColumn TRANSMISSION_PART_COLUMN_DELIVERED_AT = TimeStampColumn.JavaMSTime("DeliveredAt", true, false);
	static final public ByteArrayColumn TRANSMISSION_PART_COLUMN_BODY = new ByteArrayColumn("Body", false);
	static final public IntegerColumn TRANSMISSION_PART_COLUMN_BODY_BIT_LENGTH = new IntegerColumn("BodyBitLength", false, false, Integer.SIZE);
	//	Add columns to Transmission Part schemas & seal them:
	static
	{
		for(Schema schema : new Schema[] { SENT_TRANSMISSION_PART_SCHEMA, RECEIVED_TRANSMISSION_PART_SCHEMA } )
		{
			if(schema == SENT_TRANSMISSION_PART_SCHEMA)
				schema.addColumn(TRANSMISSION_PART_COLUMN_SENT_TRANSMISSION);
			else
				schema.addColumn(TRANSMISSION_PART_COLUMN_RECEIVED_TRANSMISSION);
			schema.addColumn(TRANSMISSION_PART_COLUMN_NUMBER);
			schema.addColumn(COLUMN_SENT_AT);
			schema.addColumn(TRANSMISSION_PART_COLUMN_DELIVERED_AT);
			schema.addColumn(COLUMN_RECEIVED_AT);
			schema.addColumn(TRANSMISSION_PART_COLUMN_BODY);
			schema.addColumn(TRANSMISSION_PART_COLUMN_BODY_BIT_LENGTH);
			schema.seal();
		}
	}
	//	Seal the model:
	static
	{
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
	
	public void storeTransmission(Transmission transmission) throws Exception
	{
		// Start transaction
		recordStore.startTransaction();
		
		try
		{
			// Use RecordGenerator to create a transmission record and part record(s):
			RecordGenerator generator = new RecordGenerator(transmission);

			// Store transmission record:
			doStoreTransmission(transmission, generator.tRec); // after this the localID should always be known
			
			// Store part records:
			for(Record tPartRec : generator.tPartRecs)
			{
				getTransmissionPartTransmissionColumn().storeValue(tPartRec, generator.tRec.getReference()); // set foreign key
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
	
	/**
	 * @param localID
	 * @return the Transmission with the given {@code localID}, or {@code null} if no such transmission was found.
	 */
	public Transmission retrieveTransmissionForID(int localID) throws Exception
	{
		return retrieveTransmissionForQuery(getTransmissionSchema().createRecordReference(localID).getRecordQuery());

	}
	
	public Transmission retrieveTransmissionForID(int localID, int payloadHash) throws Exception
	{
		return retrieveTransmissionForQuery(new FirstRecordQuery(getTransmissionSchema(), getTransmissionSchema().createRecordReference(localID).getRecordQueryConstraint(), new RuleConstraint(TRANSMISSION_COLUMN_PAYLOAD_HASH, Comparison.EQUAL, payloadHash)));
	}
	
	
	protected Transmission retrieveTransmissionForQuery(SingleRecordQuery recordQuery)
	{
		// Query for record:
		Record tRec = recordStore.retrieveRecord(recordQuery);
				
		// Null check:
		if(tRec == null)
			return null; // no such transmission found

		return transmissionFromRecord(tRec);
	}
	
	protected List<Transmission> retrieveTransmissionsForQuery(RecordsQuery multiRecordQuery)
	{
		List<Record> records = recordStore.retrieveRecords(multiRecordQuery);
		
		List<Transmission> transmissions = new ArrayList<Transmission>();
		
		for (Record record : records)
			transmissions.add(transmissionFromRecord(record));
		
		return transmissions;
	}
	
	
	private Transmission transmissionFromRecord(Record tRec)
	{
		// Values:
		Integer localID = TRANSMISSION_COLUMN_ID.retrieveValue(tRec).intValue();
		Transmission.Type type = Transmission.Type.values()[TRANSMISSION_COLUMN_TYPE.retrieveValue(tRec).intValue()]; 
		Integer remoteID = TRANSMISSION_COLUMN_REMOTE_ID.isValueSet(tRec) ? TRANSMISSION_COLUMN_REMOTE_ID.retrieveValue(tRec).intValue() : null; 
		int payloadHash = TRANSMISSION_COLUMN_PAYLOAD_HASH.retrieveValue(tRec).intValue();
		String sender = RECEIVED_TRANSMISSION_COLUMN_SENDER.retrieveValue(tRec);
		String receiver = SENT_TRANSMISSION_COLUMN_RECEIVER.retrieveValue(tRec);
		TimeStamp sentAt = COLUMN_SENT_AT.retrieveValue(tRec);
		TimeStamp receivedAt = COLUMN_RECEIVED_AT.retrieveValue(tRec);
		int totalParts = TRANSMISSION_COLUMN_NUMBER_OF_PARTS.retrieveValue(tRec).intValue();
		// Query for part records:		
		List<Record> tPartRecs = recordStore.retrieveRecords(new RecordsQuery(Source.From(getTransmissionPartSchema()), Order.AscendingBy(TRANSMISSION_PART_COLUMN_NUMBER), tRec.getRecordQueryConstraint()));
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
				binarySMS.receivePart(new BinaryMessage(binarySMS,
														TRANSMISSION_PART_COLUMN_NUMBER.retrieveValue(partRecord).intValue(),
														totalParts,
														sentAt,
														TRANSMISSION_PART_COLUMN_DELIVERED_AT.retrieveValue(partRecord),
														receivedAt,
														BitArray.FromBytes(	TRANSMISSION_PART_COLUMN_BODY.retrieveValue(partRecord),
																			TRANSMISSION_PART_COLUMN_BODY_BIT_LENGTH.retrieveValue(partRecord).intValue())));
			return binarySMS;
		case TEXTUAL_SMS:
			// create a new SMSTransmission object:
			TextSMSTransmission textSMS = new TextSMSTransmission(client, localID, remoteID, payloadHash, sentAt, receivedAt, senderAgent, receiverAgent);
			// add each part we got from the query:
			for(Record partRecord : tPartRecs)
				textSMS.receivePart(new TextMessage(textSMS, TRANSMISSION_PART_COLUMN_NUMBER.retrieveValue(partRecord).intValue(), totalParts, sentAt, TRANSMISSION_PART_COLUMN_DELIVERED_AT.retrieveValue(partRecord), receivedAt, BytesToString(TRANSMISSION_PART_COLUMN_BODY.retrieveValue(partRecord))));
			return textSMS;
		case HTTP:
			return new HTTPTransmission(client, localID, remoteID, payloadHash, sentAt, receivedAt, receiver, sender, TRANSMISSION_PART_COLUMN_BODY.retrieveValue(tPartRecs.get(0)) /* only one part for HTTP */ );
		default:
			throw new IllegalStateException("Unsupported transmission type");
		}
	}
	
	/**
	 * @param correspondent - the agent involved in the message
	 * @param remoteID - the remote agent's ID for this transmission
	 * @param payloadHash - the hash of the transmission payload
	 * @return the (first) binary SMS transmission that obeys the conditions specified by the provided arguments.
	 */
	public BinarySMSTransmission retrieveBinarySMSTransmission(SMSAgent correspondent, int remoteID, int payloadHash)
	{
		return (BinarySMSTransmission) retrieveTransmissionForQuery(new FirstRecordQuery(getTransmissionSchema(),
				new RuleConstraint(TRANSMISSION_COLUMN_TYPE, Comparison.EQUAL, Transmission.Type.BINARY_SMS.ordinal()),
				new RuleConstraint(getCorrespondentColumn(), Comparison.EQUAL, correspondent),
				new RuleConstraint(TRANSMISSION_COLUMN_REMOTE_ID, Comparison.EQUAL, remoteID),
				new RuleConstraint(TRANSMISSION_COLUMN_PAYLOAD_HASH, Comparison.EQUAL, payloadHash)));
	}
	
	/**
	 * @param correspondent - the agent involved in the message
	 * @param remoteID - the remote agent's ID for this transmission
	 * @param payloadHash - the hash of the transmission payload
	 * @return the (first) textual SMS transmission that obeys the conditions specified by the provided arguments.
	 */
	public TextSMSTransmission retrieveTextSMSTransmission(SMSAgent correspondent, int remoteID, int payloadHash)
	{ 
		return (TextSMSTransmission) retrieveTransmissionForQuery(new FirstRecordQuery(getTransmissionSchema(),
				new RuleConstraint(TRANSMISSION_COLUMN_TYPE, Comparison.EQUAL, Transmission.Type.TEXTUAL_SMS.ordinal()),
				new RuleConstraint(getCorrespondentColumn(), Comparison.EQUAL, correspondent),
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
		return (HTTPTransmission) retrieveTransmissionForQuery(new FirstRecordQuery(getTransmissionSchema(),
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
			RecordReference tRecRef = getTransmissionSchema().createRecordReference(transmission.getLocalID());
				
			// Delete transmission part records:
			recordStore.delete(new RecordsQuery(Source.From(getTransmissionPartSchema()), tRecRef.getRecordQueryConstraint()));
			
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
	
	protected abstract Schema getTransmissionSchema();

	protected abstract StringColumn getCorrespondentColumn();
	
	protected abstract Schema getTransmissionPartSchema();
	
	protected abstract ForeignKeyColumn getTransmissionPartTransmissionColumn();
	
	/**
	 * @author mstevens
	 *
	 */
	private class RecordGenerator implements Transmission.Handler, Message.Handler
	{

		public final Record tRec;
		public final List<Record> tPartRecs = new ArrayList<Record>();
		
		public RecordGenerator(Transmission transmission)
		{			
			// Create transmission record:
			tRec = getTransmissionSchema().createRecord();
			
			// Set values of all columns will be set except for Sender, Receiver & NumberOfParts:
			TRANSMISSION_COLUMN_ID.storeValue(tRec, transmission.getLocalID());
			TRANSMISSION_COLUMN_REMOTE_ID.storeValue(tRec, transmission.getRemoteID());
			TRANSMISSION_COLUMN_TYPE.storeValue(tRec, transmission.getType().ordinal());
			TRANSMISSION_COLUMN_PAYLOAD_HASH.storeValue(tRec, transmission.getPayloadHash()); // payload hash should always be set before storage
			if(transmission.isPayloadSet())
				TRANSMISSION_COLUMN_PAYLOAD_TYPE.storeValue(tRec, transmission.getPayload().getType());
			COLUMN_SENT_AT.storeValue(tRec, transmission.getSentAt());
			COLUMN_RECEIVED_AT.storeValue(tRec, transmission.getReceivedAt());
			
			// Use double dispatch for type-specific work:
			transmission.handle(this);
		}
		
		private Record newPartRecord()
		{
			Record tPartRec = getTransmissionPartSchema().createRecord();
			tPartRecs.add(tPartRec);
			return tPartRec;
		}
		
		private void handleSMS(SMSTransmission<?> smsT)
		{
			// Set SMS-specific values:
			TRANSMISSION_COLUMN_NUMBER_OF_PARTS.storeValue(tRec, smsT.getTotalNumberOfParts());
			if(smsT.isSenderSet())
				RECEIVED_TRANSMISSION_COLUMN_SENDER.storeValue(tRec, smsT.getSender().toString());
			if(smsT.isReceiverSet())
				SENT_TRANSMISSION_COLUMN_RECEIVER.storeValue(tRec, smsT.getReceiver().toString());

			// Make records for the parts...
			for(Message msg : smsT.getParts())
			{
				Record tPartRec = newPartRecord(); // adds to the list as well
				
				// Set columns (except for foreign key):
				TRANSMISSION_PART_COLUMN_NUMBER.storeValue(tPartRec, msg.getPartNumber());
				COLUMN_SENT_AT.storeValue(tPartRec, msg.getSentAt());
				TRANSMISSION_PART_COLUMN_DELIVERED_AT.storeValue(tPartRec, msg.getDeliveredAt());
				COLUMN_RECEIVED_AT.storeValue(tPartRec, msg.getReceivedAt());
				msg.handle(this); // will set part body and body bit length
			}
		}
		
		@Override
		public void handle(BinarySMSTransmission binSMST)
		{
			handleSMS(binSMST);
		}

		@Override
		public void handle(TextSMSTransmission txtSMST)
		{
			handleSMS(txtSMST);
		}

		@Override
		public void handle(BinaryMessage binMsg)
		{
			BitArray bits = binMsg.getBody();
			setPartBody(bits.toByteArray(), bits.length());
		}

		@Override
		public void handle(TextMessage txtMsg)
		{
			setPartBody(StringToBytes(txtMsg.getBody()));
		}
		
		@Override
		public void handle(HTTPTransmission httpT)
		{
			// Set receiver (= serverURL) and number of parts (always = 1):
			SENT_TRANSMISSION_COLUMN_RECEIVER.storeValue(tRec, httpT.getReceiverURL());
			TRANSMISSION_COLUMN_NUMBER_OF_PARTS.storeValue(tRec, 1);
			
			// Create a single transmission part (only used to store the body):
			Record tPartRec = newPartRecord(); // adds to the list as well
			TRANSMISSION_PART_COLUMN_NUMBER.storeValue(tPartRec, 1l); // (foreign key is not set yet)
			setPartBody(httpT.getBody()); // will set part body and body bit length
		}
		
		private void setPartBody(byte[] bodyBytes)
		{
			setPartBody(bodyBytes, bodyBytes.length * Byte.SIZE);
		}
		
		private void setPartBody(byte[] bodyBytes, int bitLength)
		{
			// Last tPartRec in the list:
			Record tPartRec = tPartRecs.get(tPartRecs.size() - 1);
			
			// Set body & body bit length columns:
			TRANSMISSION_PART_COLUMN_BODY.storeValue(tPartRec, bodyBytes);
			TRANSMISSION_PART_COLUMN_BODY_BIT_LENGTH.storeValue(tPartRec, bitLength);
		}
		
	}
	
}
