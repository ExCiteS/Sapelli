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
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
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
import uk.ac.ucl.excites.sapelli.storage.model.indexes.PrimaryKey;
import uk.ac.ucl.excites.sapelli.storage.queries.FirstRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Order;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Source;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.EqualityConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.RuleConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.RuleConstraint.Comparison;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;
import uk.ac.ucl.excites.sapelli.transmission.model.Payload;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.http.HTTPTransmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.Message;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSCorrespondent;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.binary.BinarySMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.text.TextMessage;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.text.TextSMSTransmission;

/**
 * Class to handle storage of transmissions and their parts. Based on {@link RecordStore}.
 * 
 * @author mstevens, Michalis Vitos, benelliott
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
	// Schema(s) & columns:
	//	Correspondent schemas:
	static final public Schema RECEIVER_SCHEMA = new Schema(TRANSMISSION_MANAGEMENT_MODEL, "Receiver");
	static final public Schema SENDER_SCHEMA = new Schema(TRANSMISSION_MANAGEMENT_MODEL, "Sender");
	//	Correspondent columns:
	static final public IntegerColumn CORRESPONDENT_COLUMN_ID = new IntegerColumn("ID", false, Correspondent.CORRESPONDENT_ID_FIELD);
	static final public StringColumn CORRESPONDENT_COLUMN_NAME = new StringColumn("Name", false, Correspondent.CORRESPONDENT_NAME_MAX_LENGTH_BYTES);
	static final public IntegerColumn CORRESPONDENT_COLUMN_TRANSMISSION_TYPE = new IntegerColumn("TransmissionType", false);
	static final public StringColumn CORRESPONDENT_COLUMN_ADDRESS = new StringColumn("Address", true, Correspondent.CORRESPONDENT_ADDRESS_MAX_LENGTH_BYTES);
	//static final public StringColumn CORRESPONDENT_COLUMN_ENCRYPTION_KEY = new StringColumn("Key", false, Correspondent.CORRESPONDENT_ENCRYPTION_KEY_MAX_LENGTH_BYTES);
	//	Add columns and index to Correspondent Schema & seal it:
	static
	{
		for(Schema schema : new Schema[] { RECEIVER_SCHEMA, SENDER_SCHEMA } )
		{
			schema.addColumn(CORRESPONDENT_COLUMN_ID);
			schema.addColumn(CORRESPONDENT_COLUMN_NAME);
			schema.addColumn(CORRESPONDENT_COLUMN_TRANSMISSION_TYPE);
			schema.addColumn(CORRESPONDENT_COLUMN_ADDRESS);
			//schema.addColumn(CORRESPONDENT_COLUMN_ENCRYPTION_KEY);
			schema.setPrimaryKey(new AutoIncrementingPrimaryKey(schema.getName() + "_PK", CORRESPONDENT_COLUMN_ID));
			schema.seal();
		}
	}
	//	Transmission schemas:
	static final public Schema SENT_TRANSMISSION_SCHEMA = new Schema(TRANSMISSION_MANAGEMENT_MODEL, "SentTransmission");
	static final public Schema RECEIVED_TRANSMISSION_SCHEMA = new Schema(TRANSMISSION_MANAGEMENT_MODEL, "ReceivedTransmission");
	//	Transmission columns:
	static final public IntegerColumn TRANSMISSION_COLUMN_ID = new IntegerColumn("ID", false, Transmission.TRANSMISSION_ID_FIELD);
	static final public IntegerColumn TRANSMISSION_COLUMN_REMOTE_ID = new IntegerColumn("RemoteID", true, Transmission.TRANSMISSION_ID_FIELD);
	static final public IntegerColumn TRANSMISSION_COLUMN_TYPE = new IntegerColumn("Type", false);
	static final public IntegerColumn TRANSMISSION_COLUMN_PAYLOAD_HASH = new IntegerColumn("PayloadHash", false, Transmission.PAYLOAD_HASH_FIELD);
	static final public IntegerColumn TRANSMISSION_COLUMN_PAYLOAD_TYPE = new IntegerColumn("PayloadType", true, Payload.PAYLOAD_TYPE_FIELD);
	static final public ForeignKeyColumn SENT_TRANSMISSION_COLUMN_RECEIVER = new ForeignKeyColumn("Receiver", RECEIVER_SCHEMA, false);
	static final public ForeignKeyColumn RECEIVED_TRANSMISSION_COLUMN_SENDER = new ForeignKeyColumn("Sender", SENDER_SCHEMA, false);
	static final public IntegerColumn TRANSMISSION_COLUMN_NUMBER_OF_PARTS = new IntegerColumn("NumberOfParts", false, false, Integer.SIZE);
	static final public IntegerColumn TRANSMISSION_COLUMN_NUMBER_OF_RESEND_REQS_SENT = new IntegerColumn("SentResendRequests", false, Integer.SIZE); // only used on receiving side
	static final public TimeStampColumn TRANSMISSION_COLUMN_LAST_RESEND_REQS_SENT_AT = TimeStampColumn.JavaMSTime("LastResendReqSentAt", true, false); // only used on receiving side
	//	Columns shared with Transmision Part schema:
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
				schema.addColumn(SENT_TRANSMISSION_COLUMN_RECEIVER);
			else
				schema.addColumn(RECEIVED_TRANSMISSION_COLUMN_SENDER);
			schema.addColumn(TRANSMISSION_COLUMN_NUMBER_OF_PARTS);
			schema.addColumn(COLUMN_SENT_AT);
			schema.addColumn(COLUMN_RECEIVED_AT);
			if(schema == RECEIVED_TRANSMISSION_SCHEMA)
			{
				schema.addColumn(TRANSMISSION_COLUMN_NUMBER_OF_RESEND_REQS_SENT);
				schema.addColumn(TRANSMISSION_COLUMN_LAST_RESEND_REQS_SENT_AT);
			}
			schema.setPrimaryKey(new AutoIncrementingPrimaryKey(schema.getName() + "_PK", TRANSMISSION_COLUMN_ID));
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
			schema.setPrimaryKey(PrimaryKey.WithColumnNames(
				(schema == SENT_TRANSMISSION_PART_SCHEMA ?
					TRANSMISSION_PART_COLUMN_SENT_TRANSMISSION :
					TRANSMISSION_PART_COLUMN_RECEIVED_TRANSMISSION),
				TRANSMISSION_PART_COLUMN_NUMBER));
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
	
	public void store(Correspondent correspondent) throws Exception
	{
		// Start transaction
		recordStore.startTransaction();
		
		try
		{
			doStoreCorrespondent(correspondent);
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
	 * TODO explain
	 * 
	 * @param correspondent
	 * @return
	 * @throws Exception
	 */
	private RecordReference doStoreCorrespondent(Correspondent correspondent) throws Exception
	{
		CorrespondentRecordGenerator generator = new CorrespondentRecordGenerator(correspondent);
		
		// Store the correspondent record:
		recordStore.store(generator.rec);
		//	local ID should now be set in the record...
		
		// Check/set it on the object:
		if(correspondent.isLocalIDSet()) // if the object already had a local transmissionID...
		{	// then it should match the ID on the record, so let's verify:
			if(correspondent.getLocalID() != CORRESPONDENT_COLUMN_ID.retrieveValue(generator.rec))
				throw new IllegalStateException("Non-matching correspodent ID"); // this should never happen
		}
		else
			// Set local transmissionID in object as on the record: 
			correspondent.setLocalID(CORRESPONDENT_COLUMN_ID.retrieveValue(generator.rec).intValue());
		
		return generator.rec.getReference();
	}
	
	public Correspondent retrieveCorrespondentByQuery(SingleRecordQuery recordQuery)
	{
		// Query for record and convert to Correspondent object:
		return correspondentFromRecord(recordStore.retrieveRecord(recordQuery));
	}
	
	public Record getCorrespondentRecord(Correspondent correspondent)
	{
		CorrespondentRecordGenerator generator = new CorrespondentRecordGenerator(correspondent);
		
		return generator.rec;
	}

	/**
	 * Retrieves the SMSCorrespondent with the given phone number and binary/text mode
	 * 
	 * @param phoneNumber
	 * @param binarySMS
	 * @return the correspondent or null
	 */
	public SMSCorrespondent retrieveSMSCorrespondent(String phoneNumber, boolean binarySMS)
	{
		return (SMSCorrespondent) retrieveCorrespondentByQuery(
			new FirstRecordQuery(	getCorrespondentSchema(),
									new EqualityConstraint(CORRESPONDENT_COLUMN_ADDRESS, phoneNumber),
									new RuleConstraint(	CORRESPONDENT_COLUMN_TRANSMISSION_TYPE,
														Comparison.EQUAL,
														(binarySMS ?
															Transmission.Type.BINARY_SMS :
															Transmission.Type.TEXTUAL_SMS).ordinal())));
	}
	
	// TODO delete correspondent?
	
	/**
	 * @param transmission assumed to have all values set, except the (local) ID when inserting
	 * @throws Exception 
	 */
	private void doStoreTransmission(Transmission<?> transmission, Record transmissionRecord) throws Exception
	{
		// Store the transmission record:
		recordStore.store(transmissionRecord);
		//	local ID should now be set in the record...
		
		// Check/set it on the object:
		if(transmission.isLocalIDSet()) // if the object already had a local transmissionID...
		{	// then it should match the ID on the record, so let's verify:
			if(transmission.getLocalID() != TRANSMISSION_COLUMN_ID.retrieveValue(transmissionRecord))
				throw new IllegalStateException("Non-matching transmission ID"); // this should never happen
		}
		else
			// Set local transmissionID in object as on the record: 
			transmission.setLocalID(TRANSMISSION_COLUMN_ID.retrieveValue(transmissionRecord).intValue());
	}
	
	public void store(Transmission<?> transmission) throws Exception
	{
		// Start transaction
		recordStore.startTransaction();
		
		try
		{
			// Use RecordGenerator to create a transmission record and part record(s):
			TransmissionRecordGenerator generator = new TransmissionRecordGenerator(transmission);

			// Set foreign key for Correspondent record (possibly first storing/updating it):
			getCorrespondentColumn().storeValue(generator.tRec, doStoreCorrespondent(transmission.getCorrespondent()));
			
			//System.out.println("TREC: " + generator.tRec.toString());
			
			// Store transmission record:
			doStoreTransmission(transmission, generator.tRec); // after this the localID should always be known
			
			// Store part records:
			for(Record tPartRec : generator.tPartRecs)
			{
				getTransmissionPartTransmissionColumn().storeValue(tPartRec, generator.tRec.getReference()); // set foreign key!
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
	public Transmission<?> retrieveTransmissionForID(int localID) throws Exception
	{
		return retrieveTransmissionByQuery(getTransmissionSchema().createRecordReference(localID).getRecordQuery());

	}
	
	public Transmission<?> retrieveTransmissionFor(int localID, int payloadHash) throws Exception
	{
		return retrieveTransmissionByQuery(
			new FirstRecordQuery(	getTransmissionSchema(),
									getTransmissionSchema().createRecordReference(localID).getRecordQueryConstraint(),
									new RuleConstraint(TRANSMISSION_COLUMN_PAYLOAD_HASH, Comparison.EQUAL, payloadHash)));
	}
	
	
	protected Transmission<?> retrieveTransmissionByQuery(SingleRecordQuery recordQuery)
	{
		// Query for record and convert to Transmission object:
		return transmissionFromRecord(recordStore.retrieveRecord(recordQuery));
	}
	
	protected List<Transmission<?>> retrieveTransmissions(RecordsQuery multiRecordQuery)
	{
		List<Transmission<?>> transmissions = new ArrayList<Transmission<?>>();
		for (Record record : recordStore.retrieveRecords(multiRecordQuery))
			CollectionUtils.addIgnoreNull(transmissions, transmissionFromRecord(record));
		return transmissions;
	}
	
	private Transmission<?> transmissionFromRecord(Record tRec)
	{
		// Null check:
		if(tRec == null)
			return null; // no such transmission found
		
		// Values:
		Integer localID = TRANSMISSION_COLUMN_ID.retrieveValue(tRec).intValue();
		Transmission.Type type = Transmission.Type.values()[TRANSMISSION_COLUMN_TYPE.retrieveValue(tRec).intValue()]; 
		Integer remoteID = TRANSMISSION_COLUMN_REMOTE_ID.isValueSet(tRec) ? TRANSMISSION_COLUMN_REMOTE_ID.retrieveValue(tRec).intValue() : null; 
		int payloadHash = TRANSMISSION_COLUMN_PAYLOAD_HASH.retrieveValue(tRec).intValue();
		TimeStamp sentAt = COLUMN_SENT_AT.retrieveValue(tRec);
		TimeStamp receivedAt = COLUMN_RECEIVED_AT.retrieveValue(tRec);
		int totalParts = TRANSMISSION_COLUMN_NUMBER_OF_PARTS.retrieveValue(tRec).intValue();
		// columns only occurs on receiving side:
		int numberOfSentResendRequests = isReceivingSide() ? TRANSMISSION_COLUMN_NUMBER_OF_RESEND_REQS_SENT.retrieveValue(tRec).intValue() : 0;
		TimeStamp lastResendReqSentAt =	isReceivingSide() ?	TRANSMISSION_COLUMN_LAST_RESEND_REQS_SENT_AT.retrieveValue(tRec) : null;
		
		// TODO remove debug sysos:
		System.out.println("TREC: " + tRec.toString());
		System.out.println("TREC type: " + type.name());
		
		// Query for correspondent record:
		Record cRec = recordStore.retrieveRecord(getCorrespondentColumn().retrieveValue(tRec).getRecordQuery());
		SMSCorrespondent corr = (SMSCorrespondent) correspondentFromRecord(cRec);
		
		// Query for part records:		
		List<Record> tPartRecs = recordStore.retrieveRecords(new RecordsQuery(Source.From(getTransmissionPartSchema()), Order.AscendingBy(TRANSMISSION_PART_COLUMN_NUMBER), tRec.getRecordQueryConstraint()));
		
		switch(type)
		{
			case BINARY_SMS:
				// create a new SMSTransmission object:
				BinarySMSTransmission binarySMST =  new BinarySMSTransmission(client, corr, localID, remoteID, payloadHash, sentAt, receivedAt, numberOfSentResendRequests, lastResendReqSentAt);
				// add each part we got from the query:
				for(Record tPartRec : tPartRecs)
					binarySMST.receivePart(new BinaryMessage(binarySMST,
															TRANSMISSION_PART_COLUMN_NUMBER.retrieveValue(tPartRec).intValue(),
															totalParts,
															COLUMN_SENT_AT.retrieveValue(tPartRec),
															TRANSMISSION_PART_COLUMN_DELIVERED_AT.retrieveValue(tPartRec),
															COLUMN_RECEIVED_AT.retrieveValue(tPartRec),
															BitArray.FromBytes(	TRANSMISSION_PART_COLUMN_BODY.retrieveValue(tPartRec),
																				TRANSMISSION_PART_COLUMN_BODY_BIT_LENGTH.retrieveValue(tPartRec).intValue())));
				return binarySMST;
			case TEXTUAL_SMS:
				// create a new SMSTransmission object:
				TextSMSTransmission textSMST = new TextSMSTransmission(client, corr, localID, remoteID, payloadHash, sentAt, receivedAt, numberOfSentResendRequests, lastResendReqSentAt);
				// add each part we got from the query:
				for(Record tPartRec : tPartRecs)
					textSMST.receivePart(new TextMessage(textSMST,
														TRANSMISSION_PART_COLUMN_NUMBER.retrieveValue(tPartRec).intValue(),
														totalParts,
														COLUMN_SENT_AT.retrieveValue(tPartRec),
														TRANSMISSION_PART_COLUMN_DELIVERED_AT.retrieveValue(tPartRec),
														COLUMN_RECEIVED_AT.retrieveValue(tPartRec),
														BytesToString(TRANSMISSION_PART_COLUMN_BODY.retrieveValue(tPartRec))));
				return textSMST;
			case HTTP:
				return null; // TODO !!!
				//return new HTTPTransmission(client, (SMSCorrespondent) correspondentFromRecord(cRec), localID, remoteID, payloadHash, sentAt, receivedAt, receiver, sender, TRANSMISSION_PART_COLUMN_BODY.retrieveValue(tPartRecs.get(0)) /* only one part for HTTP */ );
			default:
				throw new IllegalStateException("Unsupported transmission type");
		}
	}
	
	private Correspondent correspondentFromRecord(Record cRec)
	{
		// Null check:
		if(cRec == null)
			return null;
		
		int localID = CORRESPONDENT_COLUMN_ID.retrieveValue(cRec).intValue();
		String name = CORRESPONDENT_COLUMN_NAME.retrieveValue(cRec);
		String address = CORRESPONDENT_COLUMN_ADDRESS.retrieveValue(cRec);
		Transmission.Type ttype = Transmission.Type.values()[CORRESPONDENT_COLUMN_TRANSMISSION_TYPE.retrieveValue(cRec).intValue()];
		switch(ttype)
		{
			case BINARY_SMS:
				return new SMSCorrespondent(localID, name, address, true);
			case TEXTUAL_SMS:
				return new SMSCorrespondent(localID, name, address, false);
			case HTTP:
				return null; // TODO !!!
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
	public BinarySMSTransmission retrieveBinarySMSTransmission(SMSCorrespondent correspondent, int remoteID, int payloadHash)
	{
		// Check correspondent:
		if(!correspondent.isLocalIDSet())
			throw new IllegalStateException("Correspondent (" + correspondent.toString() + ") is unknown in database.");
		
		return (BinarySMSTransmission) retrieveTransmissionByQuery(new FirstRecordQuery(getTransmissionSchema(),
				new RuleConstraint(TRANSMISSION_COLUMN_TYPE, Comparison.EQUAL, Transmission.Type.BINARY_SMS.ordinal()),
				getCorrespondentSchema().createRecordReference(correspondent.getLocalID()).getRecordQueryConstraint(),
				new RuleConstraint(TRANSMISSION_COLUMN_REMOTE_ID, Comparison.EQUAL, remoteID),
				new RuleConstraint(TRANSMISSION_COLUMN_PAYLOAD_HASH, Comparison.EQUAL, payloadHash)));
	}
	
	/**
	 * @param correspondent - the agent involved in the message
	 * @param remoteID - the remote agent's ID for this transmission
	 * @param payloadHash - the hash of the transmission payload
	 * @return the (first) textual SMS transmission that obeys the conditions specified by the provided arguments.
	 */
	public TextSMSTransmission retrieveTextSMSTransmission(SMSCorrespondent correspondent, int remoteID, int payloadHash)
	{
		// Check correspondent:
		if(!correspondent.isLocalIDSet())
			throw new IllegalStateException("Correspondent (" + correspondent.toString() + ") is unknown in database.");
		
		return (TextSMSTransmission) retrieveTransmissionByQuery(new FirstRecordQuery(getTransmissionSchema(),
				new RuleConstraint(TRANSMISSION_COLUMN_TYPE, Comparison.EQUAL, Transmission.Type.TEXTUAL_SMS.ordinal()),
				getCorrespondentSchema().createRecordReference(correspondent.getLocalID()).getRecordQueryConstraint(),
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
		return (HTTPTransmission) retrieveTransmissionByQuery(new FirstRecordQuery(getTransmissionSchema(),
				new RuleConstraint(TRANSMISSION_COLUMN_TYPE, Comparison.EQUAL, Transmission.Type.HTTP.ordinal()),
				new RuleConstraint(TRANSMISSION_COLUMN_PAYLOAD_TYPE, Comparison.EQUAL, payloadType),
				new RuleConstraint(TRANSMISSION_COLUMN_PAYLOAD_HASH, Comparison.EQUAL, payloadHash)));
	}

	public void deleteTransmission(Transmission<?> transmission)
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
	
	protected abstract Schema getCorrespondentSchema();

	protected abstract ForeignKeyColumn getCorrespondentColumn();
	
	protected abstract Schema getTransmissionPartSchema();
	
	protected abstract ForeignKeyColumn getTransmissionPartTransmissionColumn();
	
	/**
	 * Helper class to generate Records representing Correspondents
	 * 
	 * @author mstevens
	 */
	private class CorrespondentRecordGenerator implements Correspondent.Handler
	{

		public final Record rec;
		
		public CorrespondentRecordGenerator(Correspondent correspondent)
		{
			rec = getCorrespondentSchema().createRecord();
			
			if(correspondent.isLocalIDSet())
				CORRESPONDENT_COLUMN_ID.storeValue(rec, correspondent.getLocalID());
			CORRESPONDENT_COLUMN_NAME.storeValue(rec, correspondent.getName());
			CORRESPONDENT_COLUMN_TRANSMISSION_TYPE.storeValue(rec, correspondent.getTransmissionType().ordinal());
			CORRESPONDENT_COLUMN_ADDRESS.storeValue(rec, correspondent.getAddress());
			
			// Use double dispatch for subclass-specific work:
			correspondent.handle(this);
		}
		
		@Override
		public void handle(SMSCorrespondent smsCorrespondent)
		{
			// does nothing (for now)
		}
		
	}
	
	/**
	 * Helper class to generate Records representing Transmissions and their parts (Messages)
	 * 
	 * @author mstevens
	 */
	private class TransmissionRecordGenerator implements Transmission.Handler, Message.Handler
	{

		//public final Record cRec;
		public final Record tRec;
		public final List<Record> tPartRecs = new ArrayList<Record>();
		
		public TransmissionRecordGenerator(Transmission<?> transmission)
		{			
			// Create transmission record:
			tRec = getTransmissionSchema().createRecord();
			
			// Set values of all columns will be set except for Correspondent & NumberOfParts:
			if(transmission.isLocalIDSet())
				TRANSMISSION_COLUMN_ID.storeValue(tRec, transmission.getLocalID());	
			if(transmission.isRemoteIDSet())
				TRANSMISSION_COLUMN_REMOTE_ID.storeValue(tRec, transmission.getRemoteID());
			TRANSMISSION_COLUMN_TYPE.storeValue(tRec, transmission.getType().ordinal());
			TRANSMISSION_COLUMN_PAYLOAD_HASH.storeValue(tRec, transmission.getPayloadHash()); // payload hash should always be set before storage
			if(transmission.isPayloadSet())
				TRANSMISSION_COLUMN_PAYLOAD_TYPE.storeValue(tRec, transmission.getPayload().getType());
			if(transmission.isSent())
				COLUMN_SENT_AT.storeValue(tRec, transmission.getSentAt());
			if(transmission.isReceived())
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
			if(isReceivingSide())
			{	// columns only occurs on receiving side:
				TRANSMISSION_COLUMN_NUMBER_OF_RESEND_REQS_SENT.storeValue(tRec, smsT.getNumberOfSentResendRequests());
				TRANSMISSION_COLUMN_LAST_RESEND_REQS_SENT_AT.storeValue(tRec, smsT.getLastResendRequestSentAt());
			}
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
			// Set number of parts (always = 1):
			TRANSMISSION_COLUMN_NUMBER_OF_PARTS.storeValue(tRec, 1);
			if(isReceivingSide()) // columns only occurs on receiving side
			{
				// Set number of resend requests (always = 0):
				TRANSMISSION_COLUMN_NUMBER_OF_RESEND_REQS_SENT.storeValue(tRec, 0);
				// TRANSMISSION_COLUMN_LAST_RESEND_REQS_SENT_AT remains null
			}
			
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
	
	protected abstract boolean isReceivingSide();
	
}
