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

import com.google.i18n.phonenumbers.Phonenumber.PhoneNumber;

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
public class TransmissionStore extends Store implements StoreHandle.StoreUser
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
	static final public Schema CORRESPONDENT_SCHEMA = new Schema(TRANSMISSION_MANAGEMENT_MODEL, "Correspondent");
	//	Correspondent columns:
	static final public IntegerColumn CORRESPONDENT_COLUMN_ID = new IntegerColumn("ID", false, false); // unsigned 32 bits
	static final public StringColumn CORRESPONDENT_COLUMN_NAME = StringColumn.ForCharacterCount("Name", false, Correspondent.CORRESPONDENT_NAME_MAX_LENGTH_CHARS);
	static final public IntegerColumn CORRESPONDENT_COLUMN_TRANSMISSION_TYPE = new IntegerColumn("TransmissionType", false);
	static final public StringColumn CORRESPONDENT_COLUMN_ADDRESS = StringColumn.ForCharacterCount("Address", true, Correspondent.CORRESPONDENT_ADDRESS_MAX_LENGTH_CHARS);
	//static final public StringColumn CORRESPONDENT_COLUMN_ENCRYPTION_KEY = new StringColumn("Key", false, Correspondent.CORRESPONDENT_ENCRYPTION_KEY_MAX_LENGTH_BYTES);
	//	Add columns and index to Correspondent Schema & seal it:
	static
	{
		CORRESPONDENT_SCHEMA.addColumn(CORRESPONDENT_COLUMN_ID);
		CORRESPONDENT_SCHEMA.addColumn(CORRESPONDENT_COLUMN_NAME);
		CORRESPONDENT_SCHEMA.addColumn(CORRESPONDENT_COLUMN_TRANSMISSION_TYPE);
		CORRESPONDENT_SCHEMA.addColumn(CORRESPONDENT_COLUMN_ADDRESS);
		//CORRESPONDENT_SCHEMA.addColumn(CORRESPONDENT_COLUMN_ENCRYPTION_KEY);
		CORRESPONDENT_SCHEMA.setPrimaryKey(new AutoIncrementingPrimaryKey(CORRESPONDENT_SCHEMA.getName() + "_PK", CORRESPONDENT_COLUMN_ID));
		CORRESPONDENT_SCHEMA.seal();
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
	static final public ForeignKeyColumn TRANSMISSION_COLUMN_CORRESPONDENT = new ForeignKeyColumn(CORRESPONDENT_SCHEMA, false);
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
			schema.addColumn(TRANSMISSION_COLUMN_CORRESPONDENT);
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
	 * Note: this method is public because it is called from ProjectRecordStore
	 * 
	 * @param correspondent
	 * @return
	 */
	public Record getCorrespondentRecord(Correspondent correspondent)
	{
		return new CorrespondentRecordGenerator(correspondent).rec; 
	}
	
	/**
	 * @param correspondent
	 * @return
	 * @throws Exception
	 */
	private RecordReference doStoreCorrespondent(Correspondent correspondent) throws Exception
	{
		Record rec = getCorrespondentRecord(correspondent);
		
		// Store the correspondent record:
		recordStore.store(rec);
		//	local ID should now be set in the record...
		
		// Check/set it on the object:
		if(correspondent.isLocalIDSet()) // if the object already had a local transmissionID...
		{	// then it should match the ID on the record, so let's verify:
			if(correspondent.getLocalID() != CORRESPONDENT_COLUMN_ID.retrieveValue(rec))
				throw new IllegalStateException("Non-matching correspodent ID"); // this should never happen
		}
		else
			// Set local transmissionID in object as on the record: 
			correspondent.setLocalID(CORRESPONDENT_COLUMN_ID.retrieveValue(rec).intValue());
		
		return rec.getReference();
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
	 * Note: this method is public because it is called from ProjectRecordStore
	 * 
	 * @param recordQuery
	 * @return
	 */
	public Correspondent retrieveCorrespondentByQuery(SingleRecordQuery recordQuery)
	{
		// Query for record and convert to Correspondent object:
		return correspondentFromRecord(recordStore.retrieveRecord(recordQuery));
	}

	/**
	 * Retrieves the SMSCorrespondent with the given phone number and binary/text mode
	 * 
	 * @param phoneNumber
	 * @param binarySMS
	 * @return the correspondent or null
	 */
	public SMSCorrespondent retrieveSMSCorrespondent(PhoneNumber phoneNumber, boolean binarySMS)
	{
		return (SMSCorrespondent) retrieveCorrespondentByQuery(
			new FirstRecordQuery(	CORRESPONDENT_SCHEMA,
									new EqualityConstraint(CORRESPONDENT_COLUMN_ADDRESS, SMSCorrespondent.getAddressString(phoneNumber)),
									new RuleConstraint(	CORRESPONDENT_COLUMN_TRANSMISSION_TYPE,
														Comparison.EQUAL,
														(binarySMS ?
															Transmission.Type.BINARY_SMS :
															Transmission.Type.TEXTUAL_SMS).ordinal())));
	}
	
	/**
	 * @param correspondent to delete
	 */
	public void deleteCorrespondent(Correspondent correspondent)
	{
		if(!correspondent.isLocalIDSet())
			return; // the correspondent was never stored
		try
		{
			// Get record reference:
			RecordReference cRecRef = CORRESPONDENT_SCHEMA.createRecordReference(correspondent.getLocalID());
				
			// Delete transmission part records:
			recordStore.delete(cRecRef);
		}
		catch(Exception ignore) {}
	}
	
	/**
	 * @param received if {@code true} we are dealing with transmissions that were received on the local device, if {@code false} we are dealing with transmissions created for sending from the local device to other ones
	 * @return the schema to use to create/store/retrieve a Record representation of such Transmission(s)
	 */
	private Schema getTransmissionSchema(boolean received)
	{
		return received ? RECEIVED_TRANSMISSION_SCHEMA : SENT_TRANSMISSION_SCHEMA;
	}
	
	/**
	 * @param received if {@code true} we are dealing with transmissions that were received on the local device, if {@code false} we are dealing with transmissions created for sending from the local device to other ones
	 * @return the schema to use to create/store/retrieve Record representations of parts of such Transmission(s)
	 */
	private Schema getTransmissionPartSchema(boolean received)
	{
		return received ? RECEIVED_TRANSMISSION_PART_SCHEMA : SENT_TRANSMISSION_PART_SCHEMA;
	}
	
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
			TRANSMISSION_COLUMN_CORRESPONDENT.storeValue(generator.tRec, doStoreCorrespondent(transmission.getCorrespondent()));
			
			//System.out.println("TREC: " + generator.tRec.toString());
			
			// Store transmission record:
			doStoreTransmission(transmission, generator.tRec); // after this the localID should always be known
						
			// Store part records:
			ForeignKeyColumn tCol = transmission.received ? TRANSMISSION_PART_COLUMN_RECEIVED_TRANSMISSION : TRANSMISSION_PART_COLUMN_SENT_TRANSMISSION;
			for(Record tPartRec : generator.tPartRecs)
			{
				tCol.storeValue(tPartRec, generator.tRec.getReference()); // set foreign key!
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
	 * @param received if {@code true} the transmission was received on the local device, if {@code false} it was created for sending from the local device to another one
	 * @param type
	 * @param localID
	 * @param remoteID
	 * @param correspondent
	 * @param payloadHash
	 * @param numberOfParts
	 * @return
	 * @throws IllegalStateException if the correspondent is unknown
	 */
	private RecordsQuery getTransmissionsQuery(boolean received, Transmission.Type type, Integer localID, Integer remoteID, Correspondent correspondent, Integer payloadHash, Integer numberOfParts) throws IllegalStateException
	{
		if(correspondent != null && !correspondent.isLocalIDSet())
			throw new IllegalStateException("Correspondent (" + correspondent.toString() + ") is unknown in database.");
		return new RecordsQuery(
			// schema (sent/received):
			getTransmissionSchema(received),						
			// localID:
			(localID != null ? getTransmissionSchema(received).createRecordReference(localID).getRecordQueryConstraint() : null),
			// remoteID:
			(remoteID != null ? new RuleConstraint(TRANSMISSION_COLUMN_REMOTE_ID, Comparison.EQUAL, remoteID) : null),
			// type:
			(type != null ? new RuleConstraint(TRANSMISSION_COLUMN_TYPE, Comparison.EQUAL, type.ordinal()) : null),
			// correspondent:
			(correspondent != null ? CORRESPONDENT_SCHEMA.createRecordReference(correspondent.getLocalID()).getRecordQueryConstraint() : null),
			// payload hash:
			(payloadHash != null ? new RuleConstraint(TRANSMISSION_COLUMN_PAYLOAD_HASH, Comparison.EQUAL, payloadHash) : null),
			// number of parts:
			(numberOfParts != null ? new RuleConstraint(TRANSMISSION_COLUMN_NUMBER_OF_PARTS, Comparison.EQUAL, numberOfParts) : null));
	}
	
	protected Transmission<?> retrieveTransmissionByQuery(SingleRecordQuery recordQuery)
	{
		// Query for record and convert to Transmission object:
		return transmissionFromRecord(recordStore.retrieveRecord(recordQuery));
	}
	
	/**
	 * @param multiRecordQuery
	 * @return
	 * @throws IllegalStateException when more than 1 matching Transmission is found
	 */
	protected Transmission<?> retrieveTransmissionByQuery(RecordsQuery multiRecordQuery) throws IllegalStateException
	{
		List<Transmission<?>> results = retrieveTransmissions(multiRecordQuery);
		if(results.size() > 1)
			throw new IllegalStateException("Found more than 1 matching transmission for query");
		if(results.isEmpty())
			return null;
		else
			return results.get(0);
	}
	
	protected List<Transmission<?>> retrieveTransmissions(RecordsQuery multiRecordQuery)
	{
		List<Transmission<?>> transmissions = new ArrayList<Transmission<?>>();
		for(Record record : recordStore.retrieveRecords(multiRecordQuery))
			CollectionUtils.addIgnoreNull(transmissions, transmissionFromRecord(record)); // convert to Transmission objects
		return transmissions;
	}
	
	private Transmission<?> transmissionFromRecord(Record tRec)
	{
		// Null check:
		if(tRec == null)
			return null; // no such transmission found
		
		// Values:
		boolean received = tRec.getSchema().equals(RECEIVED_TRANSMISSION_SCHEMA);
		int localID = TRANSMISSION_COLUMN_ID.retrieveValue(tRec).intValue();
		Transmission.Type type = Transmission.Type.values()[TRANSMISSION_COLUMN_TYPE.retrieveValue(tRec).intValue()]; 
		Integer remoteID = TRANSMISSION_COLUMN_REMOTE_ID.isValueSet(tRec) ? TRANSMISSION_COLUMN_REMOTE_ID.retrieveValue(tRec).intValue() : null;
		Integer payloadType = TRANSMISSION_COLUMN_PAYLOAD_TYPE.isValueSet(tRec) ? TRANSMISSION_COLUMN_PAYLOAD_TYPE.retrieveValue(tRec).intValue() : null;
		int payloadHash = TRANSMISSION_COLUMN_PAYLOAD_HASH.retrieveValue(tRec).intValue();
		TimeStamp sentAt = COLUMN_SENT_AT.retrieveValue(tRec);
		TimeStamp receivedAt = COLUMN_RECEIVED_AT.retrieveValue(tRec);
		int totalParts = TRANSMISSION_COLUMN_NUMBER_OF_PARTS.retrieveValue(tRec).intValue();
		// columns only occurs on receiving side:
		int numberOfSentResendRequests = received ? TRANSMISSION_COLUMN_NUMBER_OF_RESEND_REQS_SENT.retrieveValue(tRec).intValue() : 0;
		TimeStamp lastResendReqSentAt =	received ? TRANSMISSION_COLUMN_LAST_RESEND_REQS_SENT_AT.retrieveValue(tRec) : null;
		
		// Query for correspondent record:
		Record cRec = recordStore.retrieveRecord(TRANSMISSION_COLUMN_CORRESPONDENT.retrieveValue(tRec).getRecordQuery());
		SMSCorrespondent corr = (SMSCorrespondent) correspondentFromRecord(cRec);
		
		// Query for part records:		
		List<Record> tPartRecs = recordStore.retrieveRecords(new RecordsQuery(Source.From(getTransmissionPartSchema(received)), Order.AscendingBy(TRANSMISSION_PART_COLUMN_NUMBER), tRec.getRecordQueryConstraint()));
		
		// Instantiate Transmissions & Messages:
		switch(type)
		{
			case BINARY_SMS:
				// create a new SMSTransmission object:
				BinarySMSTransmission binarySMST =  new BinarySMSTransmission(client, corr, received, localID, remoteID, payloadType, payloadHash, sentAt, receivedAt, numberOfSentResendRequests, lastResendReqSentAt);
				// add each part we got from the query:
				for(Record tPartRec : tPartRecs)
					binarySMST.addPart(new BinaryMessage(	binarySMST,
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
				TextSMSTransmission textSMST = new TextSMSTransmission(client, corr, received, localID, remoteID, payloadType, payloadHash, sentAt, receivedAt, numberOfSentResendRequests, lastResendReqSentAt);
				// add each part we got from the query:
				for(Record tPartRec : tPartRecs)
					textSMST.addPart(new TextMessage(	textSMST,
														TRANSMISSION_PART_COLUMN_NUMBER.retrieveValue(tPartRec).intValue(),
														totalParts,
														COLUMN_SENT_AT.retrieveValue(tPartRec),
														TRANSMISSION_PART_COLUMN_DELIVERED_AT.retrieveValue(tPartRec),
														COLUMN_RECEIVED_AT.retrieveValue(tPartRec),
														BytesToString(TRANSMISSION_PART_COLUMN_BODY.retrieveValue(tPartRec))));
				return textSMST;
			case HTTP:
				return null; // TODO !!!
				//return new HTTPTransmission(client, (SMSCorrespondent) correspondentFromRecord(cRec), localID, remoteID, payloadType, payloadHash, sentAt, receivedAt, receiver, sender, TRANSMISSION_PART_COLUMN_BODY.retrieveValue(tPartRecs.get(0)) /* only one part for HTTP */ );
			default:
				throw new IllegalStateException("Unsupported transmission type");
		}
	}
	
	/**
	 * Retrieve a sent or received transmission by its local ID
	 * 
	 * @param received if {@code true} the transmission was received on the local device, if {@code false} it was created for sending from the local device to another one
	 * @param localID
	 * 
	 * @return the Transmission with the given {@code localID}, or {@code null} if no such transmission was found.
	 */
	public Transmission<?> retrieveTransmission(boolean received, int localID) throws Exception
	{
		return retrieveTransmissionByQuery(getTransmissionSchema(received).createRecordReference(localID).getRecordQuery());
	}
	
	/**
	 * @param received if {@code true} the transmission was received on the local device, if {@code false} it was created for sending from the local device to another one
	 * @param localID
	 * @param payloadHash
	 * @return
	 * @throws Exception
	 * @throws IllegalStateException when more than 1 matching Transmission is found
	 */
	public Transmission<?> retrieveTransmission(boolean received, int localID, int payloadHash) throws IllegalStateException
	{
		return retrieveTransmission(received, localID, payloadHash, null);
	}
	
	/**
	 * @param received if {@code true} the transmission was received on the local device, if {@code false} it was created for sending from the local device to another one
	 * @param localID
	 * @param payloadHash
	 * @param numberOfParts
	 * @return
	 * @throws IllegalStateException when more than 1 matching Transmission is found
	 */
	public Transmission<?> retrieveTransmission(boolean received, int localID, int payloadHash, Integer numberOfParts) throws IllegalStateException
	{
		return retrieveTransmissionByQuery(getTransmissionsQuery(received, null, localID, null, null, payloadHash, numberOfParts));
	}
	
	/**
	 * @param received
	 * @param type
	 * @param correspondent
	 * @param remoteID - not local!
	 * @param payloadHash
	 * @param numberOfParts
	 * @return
	 * @throws IllegalStateException when more than 1 matching Transmission is found or the correspondent is unknown
	 */
	public Transmission<?> retrieveTransmission(boolean received, Transmission.Type type, Correspondent correspondent, int remoteID, int payloadHash, int numberOfParts) throws IllegalStateException
	{
		return retrieveTransmissionByQuery(getTransmissionsQuery(received, type, null, remoteID, correspondent, payloadHash, numberOfParts));
	}

	/**
	 * Returns a list of recived but incomplete SMSTransmissions.
	 * 
	 * Note: this only deals with SMSTransmissions as an HTTPTransmission cannot (yet) be incomplete.
	 * 
	 * @return a list of incomplete SMSTransmissions
	 */
	public List<SMSTransmission<?>> getIncompleteSMSTransmissions()
	{
		List<SMSTransmission<?>> incompleteSMSTs = new ArrayList<SMSTransmission<?>>();
		
		// query DB for transmissions which are incomplete (have "null" as their receivedAt value):
		for(Transmission<?> t : retrieveTransmissions(new RecordsQuery(Source.From(getTransmissionSchema(true)), EqualityConstraint.IsNull(COLUMN_RECEIVED_AT))))
			if(t instanceof SMSTransmission)
				incompleteSMSTs.add((SMSTransmission<?>) t); // cast these transmissions as SMSTransmissions
		
		 return incompleteSMSTs;
	}

	public void deleteTransmission(Transmission<?> transmission)
	{
		if(!transmission.isLocalIDSet())
			return; // the transmission was never stored
		try
		{
			recordStore.startTransaction();
			
			// Get record reference:
			RecordReference tRecRef = getTransmissionSchema(transmission.received).createRecordReference(transmission.getLocalID());
			
			// Delete transmission part records:
			recordStore.delete(new RecordsQuery(Source.From(getTransmissionPartSchema(transmission.received)), tRecRef.getRecordQueryConstraint()));
			
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
			rec = CORRESPONDENT_SCHEMA.createRecord();
			
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
			tRec = getTransmissionSchema(transmission.received).createRecord();
			
			// Set values of all columns will be set except for Correspondent & NumberOfParts:
			if(transmission.isLocalIDSet())
				TRANSMISSION_COLUMN_ID.storeValue(tRec, transmission.getLocalID());	
			if(transmission.isRemoteIDSet())
				TRANSMISSION_COLUMN_REMOTE_ID.storeValue(tRec, transmission.getRemoteID());
			TRANSMISSION_COLUMN_TYPE.storeValue(tRec, transmission.getType().ordinal());
			TRANSMISSION_COLUMN_PAYLOAD_HASH.storeValue(tRec, transmission.getPayloadHash()); // payload hash should always be set before storage
			if(transmission.isPayloadTypeSet())
				TRANSMISSION_COLUMN_PAYLOAD_TYPE.storeValue(tRec, transmission.getPayloadType());
			if(transmission.isSent())
				COLUMN_SENT_AT.storeValue(tRec, transmission.getSentAt());
			if(transmission.isReceived())
				COLUMN_RECEIVED_AT.storeValue(tRec, transmission.getReceivedAt());
			
			// Use double dispatch for type-specific work:
			transmission.handle(this);
		}
		
		private Record newPartRecord(Transmission<?> transmission)
		{
			Record tPartRec = getTransmissionPartSchema(transmission.received).createRecord();
			tPartRecs.add(tPartRec);
			return tPartRec;
		}
		
		private void handleSMS(SMSTransmission<?> smsT)
		{
			// Set SMS-specific values:
			TRANSMISSION_COLUMN_NUMBER_OF_PARTS.storeValue(tRec, smsT.getTotalNumberOfParts());
			if(smsT.received)
			{	// columns only occurs on receiving side:
				TRANSMISSION_COLUMN_NUMBER_OF_RESEND_REQS_SENT.storeValue(tRec, smsT.getNumberOfSentResendRequests());
				TRANSMISSION_COLUMN_LAST_RESEND_REQS_SENT_AT.storeValue(tRec, smsT.getLastResendRequestSentAt());
			}
			// Make records for the parts...
			for(Message<?, ?> msg : smsT.getParts())
			{
				Record tPartRec = newPartRecord(smsT); // adds to the list as well
				
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
			if(httpT.received) // columns only occurs on receiving side
			{
				// Set number of resend requests (always = 0):
				TRANSMISSION_COLUMN_NUMBER_OF_RESEND_REQS_SENT.storeValue(tRec, 0);
				// TRANSMISSION_COLUMN_LAST_RESEND_REQS_SENT_AT remains null
			}
			
			// Create a single transmission part (only used to store the body):
			Record tPartRec = newPartRecord(httpT); // adds to the list as well
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
