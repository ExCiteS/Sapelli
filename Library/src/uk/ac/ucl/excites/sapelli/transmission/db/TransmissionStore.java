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

import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections4.map.LRUMap;

import com.google.i18n.phonenumbers.Phonenumber.PhoneNumber;

import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStoreWrapper;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.RecordReference;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ByteArrayColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.storage.model.indexes.AutoIncrementingPrimaryKey;
import uk.ac.ucl.excites.sapelli.storage.model.indexes.Index;
import uk.ac.ucl.excites.sapelli.storage.model.indexes.PrimaryKey;
import uk.ac.ucl.excites.sapelli.storage.queries.FirstRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Order;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.EqualityConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.RuleConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.RuleConstraint.Comparison;
import uk.ac.ucl.excites.sapelli.storage.queries.sources.Source;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStampColumn;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;
import uk.ac.ucl.excites.sapelli.transmission.model.Payload;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission.Type;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey.GeoKeyAccount;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey.GeoKeyTransmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.http.HTTPTransmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.Message;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSCorrespondent;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.binary.BinaryMessage;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.binary.BinarySMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.text.TextMessage;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.text.TextSMSTransmission;
import uk.ac.ucl.excites.sapelli.transmission.util.UnknownCorrespondentException;

/**
 * Class to handle storage of transmissions and their parts. Based on {@link RecordStore}.
 * 
 * @author mstevens, Michalis Vitos, benelliott
 */
public class TransmissionStore extends RecordStoreWrapper<TransmissionClient>
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
	static public final Model TRANSMISSION_MANAGEMENT_MODEL = new Model(TransmissionClient.TRANSMISSION_MANAGEMENT_MODEL_ID, "TransmissionManagement", TransmissionClient.SCHEMA_FLAGS_TRANSMISSION_INTERNAL);
	// Schema(s) & columns:
	//	Correspondent schemas:
	static final public Schema CORRESPONDENT_SCHEMA = TransmissionClient.CreateSchemaWithSuffixedTableName(TRANSMISSION_MANAGEMENT_MODEL, Correspondent.class.getSimpleName(), "s");
	//	Correspondent columns:
	static final public IntegerColumn CORRESPONDENT_COLUMN_ID = CORRESPONDENT_SCHEMA.addColumn(new IntegerColumn("ID", false, false)); // unsigned 32 bits
	static final public StringColumn CORRESPONDENT_COLUMN_NAME = CORRESPONDENT_SCHEMA.addColumn(StringColumn.ForCharacterCount("Name", false, Correspondent.CORRESPONDENT_NAME_MAX_LENGTH_CHARS));
	static final public IntegerColumn CORRESPONDENT_COLUMN_TRANSMISSION_TYPE = CORRESPONDENT_SCHEMA.addColumn(new IntegerColumn("TransmissionType", false));
	static final public StringColumn CORRESPONDENT_COLUMN_ADDRESS = CORRESPONDENT_SCHEMA.addColumn(StringColumn.ForCharacterCount("Address", true, Correspondent.CORRESPONDENT_ADDRESS_MAX_LENGTH_CHARS));
	//static final public StringColumn CORRESPONDENT_COLUMN_ENCRYPTION_KEY = CORRESPONDENT_SCHEMA.addColumn(new StringColumn("Key", false, Correspondent.CORRESPONDENT_ENCRYPTION_KEY_MAX_LENGTH_BYTES));
	static final public BooleanColumn CORRESPONDENT_COLUMN_USER_DELETED = CORRESPONDENT_SCHEMA.addColumn(new BooleanColumn("UserDeleted", false, Boolean.FALSE));
	//	Set primary key, add indexes & seal schema:
	static
	{
		CORRESPONDENT_SCHEMA.addIndex(new Index(CORRESPONDENT_COLUMN_TRANSMISSION_TYPE, false));
		CORRESPONDENT_SCHEMA.addIndex(new Index(CORRESPONDENT_COLUMN_USER_DELETED, false));
		CORRESPONDENT_SCHEMA.setPrimaryKey(new AutoIncrementingPrimaryKey(CORRESPONDENT_SCHEMA.getName() + "_PK", CORRESPONDENT_COLUMN_ID), true /*seal!*/);
	}
	//	Transmission schemas:
	static final public Schema SENT_TRANSMISSION_SCHEMA = TransmissionClient.CreateSchemaWithSuffixedTableName(TRANSMISSION_MANAGEMENT_MODEL, "Sent" + Transmission.class.getSimpleName(), "s");
	static final public Schema RECEIVED_TRANSMISSION_SCHEMA = TransmissionClient.CreateSchemaWithSuffixedTableName(TRANSMISSION_MANAGEMENT_MODEL, "Received" + Transmission.class.getSimpleName(), "s");
	//	Transmission columns:
	static final public IntegerColumn TRANSMISSION_COLUMN_ID = new IntegerColumn("ID", false, Transmission.TRANSMISSION_ID_FIELD);
	static final public IntegerColumn TRANSMISSION_COLUMN_REMOTE_ID = new IntegerColumn("RemoteID", true, Transmission.TRANSMISSION_ID_FIELD);
	static final public IntegerColumn TRANSMISSION_COLUMN_TYPE = new IntegerColumn("Type", false);
	static final public IntegerColumn TRANSMISSION_COLUMN_PAYLOAD_HASH = new IntegerColumn("PayloadHash", false, Transmission.PAYLOAD_HASH_FIELD);
	static final public IntegerColumn TRANSMISSION_COLUMN_PAYLOAD_TYPE = new IntegerColumn("PayloadType", true, Payload.PAYLOAD_TYPE_FIELD);
	static final public ForeignKeyColumn TRANSMISSION_COLUMN_CORRESPONDENT = new ForeignKeyColumn(CORRESPONDENT_SCHEMA, true);
	static final public IntegerColumn TRANSMISSION_COLUMN_NUMBER_OF_PARTS = new IntegerColumn("NumberOfParts", false, false, Integer.SIZE);
	static final public IntegerColumn TRANSMISSION_COLUMN_NUMBER_OF_RESEND_REQS_SENT = new IntegerColumn("SentResendRequests", false, Integer.SIZE); // only used on receiving side
	static final public TimeStampColumn TRANSMISSION_COLUMN_LAST_RESEND_REQS_SENT_AT = TimeStampColumn.JavaMSTime("LastResendReqSentAt", true, false); // only used on receiving side
	//	Columns shared with Transmission Part schema:
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
			schema.setPrimaryKey(new AutoIncrementingPrimaryKey(schema.getName() + "_PK", TRANSMISSION_COLUMN_ID), true /*seal!*/);
		}
	}
	//	Transmission Part schemas:
	static final public Schema SENT_TRANSMISSION_PART_SCHEMA = TransmissionClient.CreateSchemaWithSuffixedTableName(TRANSMISSION_MANAGEMENT_MODEL, "Sent" + Transmission.class.getSimpleName() + "Part", "s");
	static final public Schema RECEIVED_TRANSMISSION_PART_SCHEMA = TransmissionClient.CreateSchemaWithSuffixedTableName(TRANSMISSION_MANAGEMENT_MODEL, "Received" + Transmission.class.getSimpleName() + "Part", "s");
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
			schema.setPrimaryKey(
				PrimaryKey.WithColumnNames(
					(schema == SENT_TRANSMISSION_PART_SCHEMA ?
						TRANSMISSION_PART_COLUMN_SENT_TRANSMISSION :
						TRANSMISSION_PART_COLUMN_RECEIVED_TRANSMISSION),
					TRANSMISSION_PART_COLUMN_NUMBER),
				true /*seal!*/);
		}
	}
	//	Transmittable Records schema:
	static final public Schema TRANSMITTABLE_RECORDS_SCHEMA = TransmissionClient.CreateSchemaWithSuffixedTableName(TRANSMISSION_MANAGEMENT_MODEL, "Transmittable" + Record.class.getSimpleName(), "s");
	//		Columns:
	static public final ForeignKeyColumn TRANSMITTABLE_RECORDS_RECEIVER = TRANSMITTABLE_RECORDS_SCHEMA.addColumn(new ForeignKeyColumn(CORRESPONDENT_SCHEMA, false));
	static public final ForeignKeyColumn TRANSMITTABLE_RECORDS_COLUMN_SCHEMA = TRANSMITTABLE_RECORDS_SCHEMA.addColumn(new ForeignKeyColumn(Model.SCHEMA_SCHEMA, false));
	static public final ByteArrayColumn TRANSMITTABLE_RECORDS_COLUMN_PK_VALUES = TRANSMITTABLE_RECORDS_SCHEMA.addColumn(new ByteArrayColumn("PKValueBytes", false));
	static public final ForeignKeyColumn TRANSMITTABLE_RECORDS_COLUMN_TRANSMISSION = TRANSMITTABLE_RECORDS_SCHEMA.addColumn(new ForeignKeyColumn(SENT_TRANSMISSION_SCHEMA, true));
	//		Set PK and seal:
	static
	{
		TRANSMITTABLE_RECORDS_SCHEMA.setPrimaryKey(PrimaryKey.WithColumnNames(TRANSMITTABLE_RECORDS_RECEIVER, TRANSMITTABLE_RECORDS_COLUMN_SCHEMA, TRANSMITTABLE_RECORDS_COLUMN_PK_VALUES), true /*seal!*/);
	}
	//		ColumnPointers (helpers):
	static public final ColumnPointer<IntegerColumn> TRANSMITTABLE_RECORDS_CP_TRANSMISSION_ID = new ColumnPointer<IntegerColumn>(TRANSMITTABLE_RECORDS_SCHEMA, TRANSMISSION_COLUMN_ID);
	static public final ColumnPointer<IntegerColumn> TRANSMITTABLE_RECORDS_CP_SCHEMA_NUMBER = new ColumnPointer<IntegerColumn>(TRANSMITTABLE_RECORDS_SCHEMA, Model.SCHEMA_SCHEMA_NUMBER_COLUMN);
	static public final ColumnPointer<IntegerColumn> TRANSMITTABLE_RECORDS_CP_MODEL_ID = new ColumnPointer<IntegerColumn>(TRANSMITTABLE_RECORDS_SCHEMA, Model.MODEL_ID_COLUMN);
	//	Seal the model:
	static
	{
		TRANSMISSION_MANAGEMENT_MODEL.seal();
	}
	
	static private final int MAX_CACHE_SIZE = 32; 
	
	// DYNAMICS--------------------------------------------
	private final Map<Integer, Transmission<?>> sentCache;
	private final Map<Integer, Transmission<?>> receivedCache;
	
	/**
	 * @param client
	 * @throws DBException
	 */
	public TransmissionStore(TransmissionClient client) throws DBException
	{
		super(client);
		this.sentCache = Collections.synchronizedMap(new LRUMap<Integer, Transmission<?>>(MAX_CACHE_SIZE));
		this.receivedCache = Collections.synchronizedMap(new LRUMap<Integer, Transmission<?>>(MAX_CACHE_SIZE));
	}
	
	protected Map<Integer, Transmission<?>> getCache(boolean received)
	{
		return received ? receivedCache : sentCache;
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
			client.logError("Error upon storing correspondent", e);
			throw e;
		}
		
		// Commit transaction
		recordStore.commitTransaction();
	}
	
	/**
	 * @param correspondent
	 * @return
	 */
	private Record getCorrespondentRecord(Correspondent correspondent)
	{
		return new CorrespondentRecordGenerator(correspondent).rec; 
	}
	
	/**
	 * 
	 * 
	 * @param correspondent
	 * @return a RecordReference to the (now stored/updated) Correspondent Record
	 * @throws DBException
	 */
	private RecordReference doStoreCorrespondent(Correspondent correspondent) throws DBException
	{
		// Null check:
		if(correspondent == null)
			return null;
		
		Record cRec = getCorrespondentRecord(correspondent);
		
		// Store the correspondent record:
		recordStore.store(cRec);
		//	local ID should now be set in the record...
		
		// Check/set it on the object:
		if(correspondent.isLocalIDSet()) // if the object already had a local transmissionID...
		{	// then it should match the ID on the record, so let's verify:
			if(correspondent.getLocalID() != CORRESPONDENT_COLUMN_ID.retrieveValue(cRec).intValue())
				throw new IllegalStateException("Non-matching correspodent ID"); // this should never happen
		}
		else
			// Set local transmissionID in object as on the record: 
			correspondent.setLocalID(CORRESPONDENT_COLUMN_ID.retrieveValue(cRec).intValue());
		
		return cRec.getReference();
	}
	
	/**
	 * Note: this method is public because it is called from ProjectRecordStore
	 * 
	 * @param correspondent may be null (in which case this method just returns null as well)
	 * @param storeIfNeeded whether to store the Correspondent if it isn't already
	 * @param forceUpdate forces the Correspondent to be updated in the database
	 * @return a RecordReference pointing to the Record representing the Correspondent in the database, or null if it has never been stored (or is null itself)
	 * @throws DBException
	 */
	public RecordReference getCorrespondentRecordReference(Correspondent correspondent, boolean storeIfNeeded, boolean forceUpdate) throws DBException
	{
		if(correspondent == null)
			return null;
		else if(correspondent.isLocalIDSet() && !forceUpdate)
			return CORRESPONDENT_SCHEMA.createRecordReference(CORRESPONDENT_COLUMN_ID.convert(correspondent.getLocalID()));
		else if(storeIfNeeded || forceUpdate)
			return doStoreCorrespondent(correspondent);
		else
			return null;
	}
	
	@SuppressWarnings("unchecked")
	private <C extends Correspondent> C correspondentFromRecord(Record cRec)
	{
		// Null check:
		if(cRec == null)
			return null;
		
		int localID = CORRESPONDENT_COLUMN_ID.retrieveValue(cRec).intValue();
		String name = CORRESPONDENT_COLUMN_NAME.retrieveValue(cRec);
		String address = CORRESPONDENT_COLUMN_ADDRESS.retrieveValue(cRec);
		Transmission.Type ttype = Transmission.Type.values()[CORRESPONDENT_COLUMN_TRANSMISSION_TYPE.retrieveValue(cRec).intValue()];
		Correspondent corr;
		switch(ttype)
		{
			case BINARY_SMS:
				corr = new SMSCorrespondent(localID, name, address, true);
				break;
			case TEXTUAL_SMS:
				corr = new SMSCorrespondent(localID, name, address, false);
				break;
			case HTTP:
				corr = null; // TODO !!!
				break;
			case GeoKey:
				corr = new GeoKeyAccount(name, address);
				break;
			default:
				throw new IllegalStateException("Unsupported transmission type");
		}
		if(CORRESPONDENT_COLUMN_USER_DELETED.retrieveValue(cRec))
			corr.markAsUserDeleted();
		return (C) corr;
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
	 * @param includeUnknownSenders
	 * @param includeUserDeleted
	 * @return
	 */
	public List<Correspondent> retrieveCorrespondents(boolean includeUnknownSenders, boolean includeUserDeleted)
	{
		RecordsQuery query = new RecordsQuery(	Source.From(CORRESPONDENT_SCHEMA),
												(!includeUnknownSenders ? new EqualityConstraint(CORRESPONDENT_COLUMN_NAME, Correspondent.UNKNOWN_SENDER_NAME, false) : null),
												(!includeUserDeleted ? new EqualityConstraint(CORRESPONDENT_COLUMN_USER_DELETED, Boolean.FALSE) : null));
		List<Correspondent> correspondents = new ArrayList<Correspondent>();
		for(Record record : recordStore.retrieveRecords(query))
			CollectionUtils.addIgnoreNull(correspondents, correspondentFromRecord(record)); // convert to Correspondent objects
		return correspondents;
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
			if(transmission.getLocalID() != TRANSMISSION_COLUMN_ID.retrieveValue(transmissionRecord).intValue())
				throw new IllegalStateException("Non-matching transmission ID"); // this should never happen
		}
		else
			// Set local transmissionID in object as on the record: 
			transmission.setLocalID(TRANSMISSION_COLUMN_ID.retrieveValue(transmissionRecord).intValue());
		
		// Keep in cache:
		getCache(transmission.received).put(transmission.getLocalID(), transmission);
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
			RecordReference tRecRef = generator.tRec.getReference();
			for(Record tPartRec : generator.tPartRecs)
			{
				tCol.storeValue(tPartRec, tRecRef); // set foreign key!
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
	 * @throws UnknownCorrespondentException 
	 */
	private RecordsQuery getTransmissionsQuery(boolean received, Transmission.Type type, Integer localID, Integer remoteID, Correspondent correspondent, Integer payloadHash, Integer numberOfParts) throws UnknownCorrespondentException
	{
		if(correspondent != null && !correspondent.isLocalIDSet())
			throw new UnknownCorrespondentException("Correspondent (" + correspondent.toString() + ") is unknown in database.");
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
	
	/**
	 * @param multiRecordQuery
	 * @return
	 * @throws IllegalStateException when more than 1 matching Transmission is found
	 */
	protected Transmission<?> retrieveTransmissionByQuery(RecordsQuery multiRecordQuery) throws IllegalStateException
	{
		List<Transmission<?>> results = retrieveTransmissionsByQuery(multiRecordQuery);
		if(results.size() > 1)
			throw new IllegalStateException("Found more than 1 matching transmission for query");
		if(results.isEmpty())
			return null;
		else
			return results.get(0);
	}
	
	protected List<Transmission<?>> retrieveTransmissionsByQuery(RecordsQuery multiRecordQuery)
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
		
		// Essential values:
		boolean received = tRec.getSchema().equals(RECEIVED_TRANSMISSION_SCHEMA);
		int localID = TRANSMISSION_COLUMN_ID.retrieveValue(tRec).intValue();
		
		// Check cache:
		if(getCache(received).containsKey(localID))
			return getCache(received).get(localID);
		
		// Other values:
		Transmission.Type type = Transmission.Type.values()[TRANSMISSION_COLUMN_TYPE.retrieveValue(tRec).intValue()]; 
		Integer remoteID = TRANSMISSION_COLUMN_REMOTE_ID.isValuePresent(tRec) ? TRANSMISSION_COLUMN_REMOTE_ID.retrieveValue(tRec).intValue() : null;
		Integer payloadType = TRANSMISSION_COLUMN_PAYLOAD_TYPE.isValuePresent(tRec) ? TRANSMISSION_COLUMN_PAYLOAD_TYPE.retrieveValue(tRec).intValue() : null;
		int payloadHash = TRANSMISSION_COLUMN_PAYLOAD_HASH.retrieveValue(tRec).intValue();
		TimeStamp sentAt = COLUMN_SENT_AT.retrieveValue(tRec);
		TimeStamp receivedAt = COLUMN_RECEIVED_AT.retrieveValue(tRec);
		int totalParts = TRANSMISSION_COLUMN_NUMBER_OF_PARTS.retrieveValue(tRec).intValue();
		//	Columns only occurring on receiving side:
		int numberOfSentResendRequests = received ? TRANSMISSION_COLUMN_NUMBER_OF_RESEND_REQS_SENT.retrieveValue(tRec).intValue() : 0;
		TimeStamp lastResendReqSentAt =	received ? TRANSMISSION_COLUMN_LAST_RESEND_REQS_SENT_AT.retrieveValue(tRec) : null;
		
		// Query for correspondent record:
		Record cRec = TRANSMISSION_COLUMN_CORRESPONDENT.isValuePresent(tRec) ? recordStore.retrieveRecord(TRANSMISSION_COLUMN_CORRESPONDENT.retrieveValue(tRec)) : null;
		
		// Query for part records:		
		List<Record> tPartRecs = recordStore.retrieveRecords(new RecordsQuery(Source.From(getTransmissionPartSchema(received)), Order.AscendingBy(TRANSMISSION_PART_COLUMN_NUMBER), tRec.getRecordQueryConstraint()));
		
		// Instantiate Transmissions & Messages:
		switch(type)
		{
			case BINARY_SMS:
				// create a new SMSTransmission object:
				BinarySMSTransmission binarySMST =  new BinarySMSTransmission(client, this.<SMSCorrespondent> correspondentFromRecord(cRec), received, localID, remoteID, payloadType, payloadHash, sentAt, receivedAt, numberOfSentResendRequests, lastResendReqSentAt);
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
				TextSMSTransmission textSMST = new TextSMSTransmission(client, this.<SMSCorrespondent> correspondentFromRecord(cRec), received, localID, remoteID, payloadType, payloadHash, sentAt, receivedAt, numberOfSentResendRequests, lastResendReqSentAt);
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
				//return new HTTPTransmission(client, (HTTPServer) correspondentFromRecord(cRec), localID, remoteID, payloadType, payloadHash, sentAt, receivedAt, receiver, sender, TRANSMISSION_PART_COLUMN_BODY.retrieveValue(tPartRecs.get(0)) /* only one part for HTTP */ );
			case GeoKey:
				return new GeoKeyTransmission(client, this.<GeoKeyAccount> correspondentFromRecord(cRec), received, localID, remoteID, payloadType, payloadHash, lastResendReqSentAt, receivedAt, TRANSMISSION_PART_COLUMN_BODY.retrieveValue(tPartRecs.get(0)));
			default:
				throw new IllegalStateException("Unsupported transmission type");
		}
	}
	
	/**
	 * Retrieve a sent or received transmission by its local ID.
	 * 
	 * @param received if {@code true} the transmission was received on the local device, if {@code false} it was created for sending from the local device to another one
	 * @param localID
	 * 
	 * @return the Transmission with the given {@code localID}, or {@code null} if no such transmission was found.
	 * @throws Exception
	 */
	public Transmission<?> retrieveTransmission(boolean received, int localID) throws Exception
	{
		// Check cache:
		if(getCache(received).containsKey(localID))
			return getCache(received).get(localID);
		// else:
		return transmissionFromRecord(recordStore.retrieveRecord(getTransmissionSchema(received).createRecordReference(localID)));
	}
	
	/**
	 * @param received
	 * @param localID
	 * @return
	 */
	public Transmission<?> retrieveTransmissionOrNull(boolean received, int localID)
	{
		try
		{
			return retrieveTransmission(received, localID);
		}
		catch(Exception e)
		{
			client.logError("Error retrieving " + (received ? "received" : "sent") + " transmission with local ID = " + localID + ".", e);
			return null;
		}
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
	 * Retrieve a sent or received SMS transmission by its type (binary/textual), correspondent, remote(!) ID, payload hash, and number of parts.
	 * 
	 * @param received
	 * @param type
	 * @param correspondent
	 * @param remoteID - not local!
	 * @param payloadHash
	 * @param numberOfParts
	 * @return
	 * @throws IllegalStateException when more than 1 matching Transmission is found
	 * @throws UnknownCorrespondentException when the correspondent is unknown
	 */
	public Transmission<?> retrieveTransmission(boolean received, Transmission.Type type, Correspondent correspondent, int remoteID, int payloadHash, int numberOfParts) throws IllegalStateException, UnknownCorrespondentException
	{
		return retrieveTransmissionByQuery(getTransmissionsQuery(received, type, null, remoteID, correspondent, payloadHash, numberOfParts));
	}
	
	/**
	 * Retrieve a sent or received SMS transmission by its local ID, type (binary/textual) and number of parts.
	 * 
	 * @param received if {@code true} the transmission was received on the local device, if {@code false} it was created for sending from the local device to another one
	 * @param localID
	 * @param binary
	 * @param numberOfParts
	 * 
	 * @return the Transmission with the given {@code localID}, or {@code null} if no such transmission was found.
	 */
	public SMSTransmission<?> retrieveSMSTransmission(boolean received, int localID, boolean binary, int numberOfParts) throws Exception
	{
		return (SMSTransmission<?>) retrieveTransmissionByQuery(getTransmissionsQuery(received, binary ? Type.BINARY_SMS : Type.TEXTUAL_SMS, localID, null, null, null, numberOfParts));
	}

	/**
	 * Returns a list of received but incomplete SMSTransmissions.
	 * 
	 * Note: this only deals with SMSTransmissions as an HTTPTransmission cannot (yet) be incomplete.
	 * 
	 * @return a list of incomplete SMSTransmissions
	 */
	public List<SMSTransmission<?>> retrieveIncompleteSMSTransmissions()
	{
		List<SMSTransmission<?>> incompleteSMSTs = new ArrayList<SMSTransmission<?>>();
		
		// query DB for transmissions which are incomplete (have "null" as their receivedAt value):
		for(Transmission<?> t : retrieveTransmissionsByQuery(new RecordsQuery(Source.From(getTransmissionSchema(true)), EqualityConstraint.IsNull(COLUMN_RECEIVED_AT))))
			if(t instanceof SMSTransmission)
				incompleteSMSTs.add((SMSTransmission<?>) t); // cast these transmissions as SMSTransmissions
		
		 return incompleteSMSTs;
	}
	
	/**
	 * Returns a list of all receiver or sent transmissions from or to the given correspondent.
	 * 
	 * @param received
	 * @param correspondent
	 * @return
	 */
	public List<Transmission<?>> retrieveTransmissions(boolean received, Correspondent correspondent)
	{
		try
		{
			return retrieveTransmissionsByQuery(getTransmissionsQuery(received, null, null, null, correspondent, null, null));
		}
		catch(UnknownCorrespondentException uce)
		{
			return Collections.<Transmission<?>> emptyList();
		}
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

	/**
	 * Registers that a Record, indicated by the given RecordReference, is transmittable to the given Correspondent,
	 * and optionally that a transmission (attempt) will take or has taken place using the given Transmission object. 
	 * 
	 * @param correspondent
	 * @param recordReference a RecordReference pointing to the Record which we are told is transmittable
	 * @param transmission may be null, but if it isn't it must have been stored before
	 */
	public void storeTransmittableRecord(Correspondent correspondent, RecordReference recordReference, Transmission<?> transmission)
	{
		if(transmission != null && !transmission.isLocalIDSet())
			throw new IllegalArgumentException("Transmission must have been stored before being associated with records to need sending or have been sent.");
		try
		{
			recordStore.store(TRANSMITTABLE_RECORDS_SCHEMA.createRecord(
				// Receiver column (first store/update the Correspondent if necessary):
				getCorrespondentRecordReference(correspondent, true, false),
				// Schema column (= Model ID + Schema#):
				recordReference.getReferencedSchema().getMetaRecordReference(),
				// PKValues column:
				recordReference.toBytes(true),
				// Transmission column:
				transmission != null ? getTransmissionSchema(false).createRecordReference(transmission.getLocalID()) : null));
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err); // TODO
		}
	}
	
	/**
	 * Removes all entries relating to the referenced Record from the TransmittableRecords table (possibly for multiple receivers). 
	 * 
	 * @param recordReference
	 */
	public void deleteTransmittableRecord(RecordReference recordReference)
	{
		try
		{
			recordStore.delete(new RecordsQuery(	TRANSMITTABLE_RECORDS_SCHEMA, 
													// Schema column (= Model ID + Schema#):
													recordReference.getReferencedSchema().getMetaRecordReference().getRecordQueryConstraint(),
													// PKValueBytes column:
													new EqualityConstraint(TRANSMITTABLE_RECORDS_COLUMN_PK_VALUES, recordReference.toBytes(true))));
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err); // TODO
		}
	}
	
	/**
	 * Retrieves all records, with Schemata from the given Model, that are marked for transmission
	 * to the given Correspondent and which are not (yet) associated with a Transmission.
	 * 
	 * @param correspondent
	 * @param model
	 * @return
	 */
	public List<Record> retrieveTransmittableRecordsWithoutTransmission(Correspondent correspondent, Model model)
	{
		return retrieveTransmittableUserRecords(correspondent, model, EqualityConstraint.IsNull(TRANSMITTABLE_RECORDS_COLUMN_TRANSMISSION));
	}
	
	/**
	 * Retrieves all records, with Schemata from the given Model, that are marked for transmission
	 * to the given Correspondent and which are (already) associated with a Transmission. 
	 * 
	 * @param correspondent
	 * @param model
	 * @return
	 */
	public List<Record> retrieveTransmittableRecordsWithTransmission(Correspondent correspondent, Model model)
	{
		return retrieveTransmittableUserRecords(correspondent, model, EqualityConstraint.IsNotNull(TRANSMITTABLE_RECORDS_COLUMN_TRANSMISSION));
	}
	
	/**
	 * Retrieves all records that are marked for transmission and which are associated with the given Transmission
	 * (and therefore intended for its receiver).
	 * Note that the result should be the (bar order) as getting the records from the Transmission's RecordsPayload.
	 * 
	 * TODO will we really need this?
	 * 
	 * @param correspondent
	 * @param model
	 * @param transmission
	 * @return
	 */
	public List<Record> retrieveTransmittableRecordsWithTransmission(Model model, Transmission<?> transmission)
	{
		return retrieveTransmittableUserRecords(transmission.getCorrespondent(), model, getTransmissionSchema(false).createRecordReference(transmission.getLocalID()).getRecordQueryConstraint());
	}
	
	/**
	 * @param correspondent
	 * @param model
	 * @param timeOutS
	 * @return
	 */
	public List<Record> retrieveTransmittableRecordsWithTimedOutTransmission(Correspondent correspondent, Model model, long timeOutS)
	{
		// Get all transmittables with an assigned transmission, ordered by the transmission:
		List<Record> toSendRecs = retrieveTransmittableRecords(correspondent, model, Order.By(TRANSMITTABLE_RECORDS_COLUMN_TRANSMISSION), EqualityConstraint.IsNotNull(TRANSMITTABLE_RECORDS_COLUMN_TRANSMISSION));
		
		// Collection to return:
		List<Record> userRecs = new ArrayList<Record>(toSendRecs.size());
		
		// Loop over all transmittables:
		Transmission<?> transmission = null;
		boolean timedOut = false;
		for(Record toSendRec : toSendRecs)
		{
			int tID = ((Long) TRANSMITTABLE_RECORDS_CP_TRANSMISSION_ID.retrieveValue(toSendRec)).intValue();
			if(transmission == null || transmission.getLocalID() != tID)
			{
				transmission = retrieveTransmissionOrNull(false, tID);
				timedOut =		transmission == null 	// unknown transmission
							||	!transmission.isSent()	// never actually sent
							||	(!transmission.isReceived() && transmission.getSentAt().shift(timeOutS * 1000).isBefore(TimeStamp.now())); // never received (or no ACK received) and sent longer ago than timeoutS
			}
			if(timedOut)
				CollectionUtils.addIgnoreNull(userRecs, getUserRecordFromTransmittable(toSendRec, model));
		}
		
		return userRecs;
	}
	
	/**
	 * @param correspondent
	 * @param model
	 * @param tranmissionConstraint may be null (when querying for ToSend records regardless of whether or not they are associated with a transmission)
	 * @return
	 */
	private List<Record> retrieveTransmittableRecords(Correspondent correspondent, Model model, Order order, Constraint transmissionConstraint)
	{
		RecordReference cRecRef = null;
		try
		{
			cRecRef = getCorrespondentRecordReference(correspondent, false, false);
		}
		catch(Exception ignore) {}
		if(cRecRef == null) // this means it has never been stored so there can also be no ToSend records for it
			return Collections.<Record> emptyList();
		
		// Query for ToSend records:
		List<Record> toSendRecs = recordStore.retrieveRecords(
			new RecordsQuery(	TRANSMITTABLE_RECORDS_SCHEMA,
								order,
								cRecRef.getRecordQueryConstraint(),
								model.getModelRecordReference().getRecordQueryConstraint(),
								transmissionConstraint));
		
		// Return result:
		return toSendRecs;
	}
	
	/**
	 * @param correspondent
	 * @param model
	 * @param tranmissionConstraint may be null (when querying for ToSend records regardless of whether or not they are associated with a transmission)
	 * @return
	 */
	private List<Record> retrieveTransmittableUserRecords(Correspondent correspondent, Model model, Constraint transmissionConstraint)
	{
		// Query for ToSend records:
		List<Record> toSendRecs = retrieveTransmittableRecords(correspondent, model, Order.By(TRANSMITTABLE_RECORDS_CP_SCHEMA_NUMBER), transmissionConstraint);
		
		// Query for the actual records being referred to:
		List<Record> userRecs = new ArrayList<Record>(toSendRecs.size());
		for(Record toSendRec : toSendRecs)
			CollectionUtils.addIgnoreNull(userRecs, getUserRecordFromTransmittable(toSendRec, model));
		
		// Return result:
		return userRecs;
	}
	
	private Record getUserRecordFromTransmittable(Record toSendRecord, Model recycleModel)
	{
		try
		{
			// Get schema info:
			long modelID = ((Long) TRANSMITTABLE_RECORDS_CP_MODEL_ID.retrieveValue(toSendRecord)).longValue();
			int schemaNumber = ((Long) TRANSMITTABLE_RECORDS_CP_SCHEMA_NUMBER.retrieveValue(toSendRecord)).intValue();
			// Get or recycle schema object:
			Schema schema =
				(recycleModel != null && recycleModel.id == modelID) ?
					recycleModel.getSchema(schemaNumber) :
					client.getSchema(modelID, schemaNumber);
			// Query for & return user record:
			return recordStore.retrieveRecord(schema.createRecordReference(TRANSMITTABLE_RECORDS_COLUMN_PK_VALUES.retrieveValue(toSendRecord)));	
		}
		catch(Exception e)
		{
			client.logError("Failed to retrieve user record for transmittable entry: " + toSendRecord.toString(false), e);
			return null;
		}
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
			CORRESPONDENT_COLUMN_USER_DELETED.storeValue(rec, correspondent.isUserDeleted());
			
			// Use double dispatch for subclass-specific work:
			correspondent.handle(this);
		}
		
		@Override
		public void handle(SMSCorrespondent smsCorrespondent)
		{
			// does nothing (for now)
		}

		@Override
		public void handle(GeoKeyAccount geokeyAccount)
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
		
		public void handleHTTPLike(Transmission<?> transmission, byte[] body)
		{
			// Set number of parts (always = 1):
			TRANSMISSION_COLUMN_NUMBER_OF_PARTS.storeValue(tRec, 1);
			if(transmission.received) // columns only occurs on receiving side
			{
				// Set number of resend requests (always = 0):
				TRANSMISSION_COLUMN_NUMBER_OF_RESEND_REQS_SENT.storeValue(tRec, 0);
				// TRANSMISSION_COLUMN_LAST_RESEND_REQS_SENT_AT remains null
			}
			
			// Create a single transmission part (only used to store the body):
			Record tPartRec = newPartRecord(transmission); // adds to the list as well
			TRANSMISSION_PART_COLUMN_NUMBER.storeValue(tPartRec, 1l); // (foreign key is not set yet)
			setPartBody(body); // will set part body and body bit length
		}
		
		@Override
		public void handle(HTTPTransmission httpT)
		{
			handleHTTPLike(httpT, httpT.getBody());
		}

		@Override
		public void handle(GeoKeyTransmission geoKeyT)
		{
			handleHTTPLike(geoKeyT, geoKeyT.getBody());
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
