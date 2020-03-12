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

package uk.ac.ucl.excites.sapelli.transmission.db;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections4.map.LRUMap;
import org.apache.commons.io.Charsets;

import com.google.i18n.phonenumbers.Phonenumber.PhoneNumber;

import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.io.BitArray;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.shared.util.Objects;
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
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.AndConstraint;
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
import uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey.GeoKeyServer;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.geokey.GeoKeyTransmission;
import uk.ac.ucl.excites.sapelli.transmission.util.UnknownCorrespondentException;

/**
 * Class to handle storage of transmissions and their parts. Based on {@link RecordStore}.
 * 
 * @author mstevens, Michalis Vitos, benelliott
 */
public class TransmissionStore extends RecordStoreWrapper<TransmissionClient>
{
	
	// STATICS---------------------------------------------
	static private byte[] StringToBytes(String str)
	{
		return str.getBytes(Charsets.UTF_8);
	}
	
	static private String BytesToString(byte[] bytes)
	{
		return new String(bytes, Charsets.UTF_8);
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
	static final public Schema OUTGOING_TRANSMISSION_SCHEMA = TransmissionClient.CreateSchemaWithSuffixedTableName(TRANSMISSION_MANAGEMENT_MODEL, "Outgoing" + Transmission.class.getSimpleName(), "s");
	static final public Schema INCOMING_TRANSMISSION_SCHEMA = TransmissionClient.CreateSchemaWithSuffixedTableName(TRANSMISSION_MANAGEMENT_MODEL, "Incoming" + Transmission.class.getSimpleName(), "s");
	//	Transmission columns:
	static final public IntegerColumn TRANSMISSION_COLUMN_ID = new IntegerColumn("ID", false, Transmission.TRANSMISSION_ID_FIELD);
	static final public IntegerColumn TRANSMISSION_COLUMN_REMOTE_ID = new IntegerColumn("RemoteID", true, Transmission.TRANSMISSION_ID_FIELD);
	static final public IntegerColumn TRANSMISSION_COLUMN_TYPE = new IntegerColumn("Type", false);
	static final public IntegerColumn TRANSMISSION_COLUMN_PAYLOAD_HASH = new IntegerColumn("PayloadHash", false, Transmission.PAYLOAD_HASH_FIELD);
	static final public IntegerColumn TRANSMISSION_COLUMN_PAYLOAD_TYPE = new IntegerColumn("PayloadType", true, Payload.PAYLOAD_TYPE_FIELD);
	static final public ForeignKeyColumn TRANSMISSION_COLUMN_CORRESPONDENT = new ForeignKeyColumn(CORRESPONDENT_SCHEMA, true);
	static final public IntegerColumn TRANSMISSION_COLUMN_NUMBER_OF_PARTS = new IntegerColumn("NumberOfParts", false, false, Integer.SIZE);
	static final public String TRANSMISSION_COLUMN_NAME_RESPONSE = "Response";
	static final public BooleanColumn TRANSMISSION_COLUMN_DELETED = new BooleanColumn("Deleted", false, Boolean.FALSE);
	static final public IntegerColumn TRANSMISSION_COLUMN_NUMBER_OF_RESEND_REQS_SENT = new IntegerColumn("SentResendRequests", false, Integer.SIZE); // only used on receiving side
	static final public TimeStampColumn TRANSMISSION_COLUMN_LAST_RESEND_REQS_SENT_AT = TimeStampColumn.JavaMSTime("LastResendReqSentAt", true, false); // only used on receiving side
	//	Columns shared with Transmission Part schema:
	static final public TimeStampColumn COLUMN_SENT_AT = TimeStampColumn.JavaMSTime("SentAt", true, false);
	static final public TimeStampColumn COLUMN_RECEIVED_AT = TimeStampColumn.JavaMSTime("ReceivedAt", true, false);
	//	Add columns and index to Transmission schemas & seal them:
	static
	{
		// PKs:
		OUTGOING_TRANSMISSION_SCHEMA.addColumn(TRANSMISSION_COLUMN_ID);
		OUTGOING_TRANSMISSION_SCHEMA.setPrimaryKey(new AutoIncrementingPrimaryKey(OUTGOING_TRANSMISSION_SCHEMA.getName() + "_PK", TRANSMISSION_COLUMN_ID));
		INCOMING_TRANSMISSION_SCHEMA.addColumn(TRANSMISSION_COLUMN_ID);
		INCOMING_TRANSMISSION_SCHEMA.setPrimaryKey(new AutoIncrementingPrimaryKey(INCOMING_TRANSMISSION_SCHEMA.getName() + "_PK", TRANSMISSION_COLUMN_ID));
		// Other columns:
		for(Schema schema : new Schema[] { OUTGOING_TRANSMISSION_SCHEMA, INCOMING_TRANSMISSION_SCHEMA } )
		{
			schema.addColumn(TRANSMISSION_COLUMN_REMOTE_ID);
			schema.addColumn(TRANSMISSION_COLUMN_TYPE);
			schema.addColumn(TRANSMISSION_COLUMN_PAYLOAD_HASH);
			schema.addColumn(TRANSMISSION_COLUMN_PAYLOAD_TYPE);
			schema.addColumn(TRANSMISSION_COLUMN_CORRESPONDENT);
			schema.addColumn(TRANSMISSION_COLUMN_NUMBER_OF_PARTS);
			schema.addColumn(COLUMN_SENT_AT);
			schema.addColumn(COLUMN_RECEIVED_AT);
			// Response FK:
			schema.addColumn(new ForeignKeyColumn(TRANSMISSION_COLUMN_NAME_RESPONSE, schema == OUTGOING_TRANSMISSION_SCHEMA ? INCOMING_TRANSMISSION_SCHEMA : OUTGOING_TRANSMISSION_SCHEMA, true));
			schema.addColumn(TRANSMISSION_COLUMN_DELETED);
			// Only for incoming transmissions:
			if(schema == INCOMING_TRANSMISSION_SCHEMA)
			{
				schema.addColumn(TRANSMISSION_COLUMN_NUMBER_OF_RESEND_REQS_SENT);
				schema.addColumn(TRANSMISSION_COLUMN_LAST_RESEND_REQS_SENT_AT);
			}
			schema.seal(); // !!!
		}
	}
	//	Transmission Part schemas:
	static final public Schema OUTGOING_TRANSMISSION_PART_SCHEMA = TransmissionClient.CreateSchemaWithSuffixedTableName(TRANSMISSION_MANAGEMENT_MODEL, "Outgoing" + Transmission.class.getSimpleName() + "Part", "s");
	static final public Schema INCOMING_TRANSMISSION_PART_SCHEMA = TransmissionClient.CreateSchemaWithSuffixedTableName(TRANSMISSION_MANAGEMENT_MODEL, "Incoming" + Transmission.class.getSimpleName() + "Part", "s");
	//	Transmission Part columns:
	static final public ForeignKeyColumn TRANSMISSION_PART_COLUMN_OUTGOING_TRANSMISSION = new ForeignKeyColumn(OUTGOING_TRANSMISSION_SCHEMA, false);
	static final public ForeignKeyColumn TRANSMISSION_PART_COLUMN_INCOMING_TRANSMISSION = new ForeignKeyColumn(INCOMING_TRANSMISSION_SCHEMA, false);
	static final public IntegerColumn TRANSMISSION_PART_COLUMN_NUMBER = new IntegerColumn("PartNumber", false, false, Integer.SIZE);
	static final public TimeStampColumn TRANSMISSION_PART_COLUMN_DELIVERED_AT = TimeStampColumn.JavaMSTime("DeliveredAt", true, false);
	static final public ByteArrayColumn TRANSMISSION_PART_COLUMN_BODY = new ByteArrayColumn("Body", false);
	static final public IntegerColumn TRANSMISSION_PART_COLUMN_BODY_BIT_LENGTH = new IntegerColumn("BodyBitLength", false, false, Integer.SIZE);
	//	Add columns to Transmission Part schemas & seal them:
	static
	{
		for(Schema schema : new Schema[] { OUTGOING_TRANSMISSION_PART_SCHEMA, INCOMING_TRANSMISSION_PART_SCHEMA } )
		{
			if(schema == OUTGOING_TRANSMISSION_PART_SCHEMA)
				schema.addColumn(TRANSMISSION_PART_COLUMN_OUTGOING_TRANSMISSION);
			else
				schema.addColumn(TRANSMISSION_PART_COLUMN_INCOMING_TRANSMISSION);
			schema.addColumn(TRANSMISSION_PART_COLUMN_NUMBER);
			schema.addColumn(COLUMN_SENT_AT);
			schema.addColumn(TRANSMISSION_PART_COLUMN_DELIVERED_AT);
			schema.addColumn(COLUMN_RECEIVED_AT);
			schema.addColumn(TRANSMISSION_PART_COLUMN_BODY);
			schema.addColumn(TRANSMISSION_PART_COLUMN_BODY_BIT_LENGTH);
			schema.setPrimaryKey(
				PrimaryKey.WithColumnNames(
					(schema == OUTGOING_TRANSMISSION_PART_SCHEMA ?
						TRANSMISSION_PART_COLUMN_OUTGOING_TRANSMISSION :
						TRANSMISSION_PART_COLUMN_INCOMING_TRANSMISSION),
					TRANSMISSION_PART_COLUMN_NUMBER),
				true /*seal!*/);
		}
	}
	//	Transmittable Records schema:
	static final public Schema TRANSMITTABLE_RECORDS_SCHEMA = TransmissionClient.CreateSchemaWithSuffixedTableName(TRANSMISSION_MANAGEMENT_MODEL, "Transmittable" + Record.class.getSimpleName(), "s");
	//		Columns:
	static public final ForeignKeyColumn TRANSMITTABLE_RECORDS_COLUMN_RECEIVER = TRANSMITTABLE_RECORDS_SCHEMA.addColumn(new ForeignKeyColumn(CORRESPONDENT_SCHEMA, false));
	static public final ForeignKeyColumn TRANSMITTABLE_RECORDS_COLUMN_SCHEMA = TRANSMITTABLE_RECORDS_SCHEMA.addColumn(new ForeignKeyColumn(Model.SCHEMA_SCHEMA, false));
	static public final ByteArrayColumn TRANSMITTABLE_RECORDS_COLUMN_PK_VALUES = TRANSMITTABLE_RECORDS_SCHEMA.addColumn(new ByteArrayColumn("PKValueBytes", false));
	static public final ForeignKeyColumn TRANSMITTABLE_RECORDS_COLUMN_TRANSMISSION = TRANSMITTABLE_RECORDS_SCHEMA.addColumn(new ForeignKeyColumn(OUTGOING_TRANSMISSION_SCHEMA, true));
	static final public BooleanColumn TRANSMITTABLE_RECORDS_COLUMN_RECEIVED = TRANSMITTABLE_RECORDS_SCHEMA.addColumn(new BooleanColumn("Received", false, Boolean.FALSE));
	//		Set PK and seal:
	static
	{
		TRANSMITTABLE_RECORDS_SCHEMA.setPrimaryKey(PrimaryKey.WithColumnNames(TRANSMITTABLE_RECORDS_COLUMN_RECEIVER, TRANSMITTABLE_RECORDS_COLUMN_SCHEMA, TRANSMITTABLE_RECORDS_COLUMN_PK_VALUES), true /*seal!*/);
	}
	//		ColumnPointers (helpers):
	static public final ColumnPointer<IntegerColumn> TRANSMITTABLE_RECORDS_CP_TRANSMISSION_ID = new ColumnPointer<IntegerColumn>(TRANSMITTABLE_RECORDS_SCHEMA, TRANSMISSION_COLUMN_ID);
	static public final ColumnPointer<IntegerColumn> TRANSMITTABLE_RECORDS_CP_MODEL_ID = new ColumnPointer<IntegerColumn>(TRANSMITTABLE_RECORDS_SCHEMA, Model.MODEL_ID_COLUMN);
	static public final ColumnPointer<IntegerColumn> TRANSMITTABLE_RECORDS_CP_SCHEMA_NUMBER = new ColumnPointer<IntegerColumn>(TRANSMITTABLE_RECORDS_SCHEMA, Model.SCHEMA_SCHEMA_NUMBER_COLUMN);
	//	Seal the model:
	static
	{
		TRANSMISSION_MANAGEMENT_MODEL.seal();
	}
	
	static private final int MAX_CACHE_SIZE = 8;
	
	static public TimeStamp retrieveTimeStamp(TimeStampColumn column, Record record)
	{
		return TimeStamp.setLocalTimeZone(column.retrieveValue(record));
	}
	
	// DYNAMICS--------------------------------------------
	private final Map<Integer, Transmission<?>> outCache;
	private final Map<Integer, Transmission<?>> inCache;
	
	private final TransmissionRecordGenerator generator = new TransmissionRecordGenerator();
	
	/**
	 * @param client
	 * @throws DBException
	 */
	public TransmissionStore(TransmissionClient client) throws DBException
	{
		super(client);
		this.outCache = Collections.synchronizedMap(new LRUMap<Integer, Transmission<?>>(MAX_CACHE_SIZE));
		this.inCache = Collections.synchronizedMap(new LRUMap<Integer, Transmission<?>>(MAX_CACHE_SIZE));
	}
	
	protected Map<Integer, Transmission<?>> getCache(boolean incoming)
	{
		return incoming ? inCache : outCache;
	}
	
	public void store(Correspondent correspondent) throws DBException
	{
		// Start transaction
		recordStore.startTransaction();
		
		try
		{
			doStoreCorrespondent(correspondent);
		}
		catch(DBException e)
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
		
		//client.logInfo("Stored correspondent: " + correspondent.getName() + " (localid: " + (correspondent.isLocalIDSet() ? correspondent.getLocalID() : "null") + ")");
		
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
		else if(correspondent.isLocalIDSet() /*already stored*/ && !forceUpdate)
			return CORRESPONDENT_SCHEMA.createRecordReference(correspondent.getLocalID());
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
			case GeoKey:
				corr = new GeoKeyServer(localID, name, address);
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
	 * @param incoming if {@code true} we are dealing with transmissions that were received on the local device, if {@code false} we are dealing with transmissions created for sending from the local device to other ones
	 * @return the schema to use to create/store/retrieve a Record representation of such Transmission(s)
	 */
	private Schema getTransmissionSchema(boolean incoming)
	{
		return incoming ? INCOMING_TRANSMISSION_SCHEMA : OUTGOING_TRANSMISSION_SCHEMA;
	}
	
	/**
	 * @param incoming if {@code true} we are dealing with transmissions that were received on the local device, if {@code false} we are dealing with transmissions created for sending from the local device to other ones
	 * @return the schema to use to create/store/retrieve Record representations of parts of such Transmission(s)
	 */
	private Schema getTransmissionPartSchema(boolean incoming)
	{
		return incoming ? INCOMING_TRANSMISSION_PART_SCHEMA : OUTGOING_TRANSMISSION_PART_SCHEMA;
	}
	
	/**
	 * @param incoming if {@code true} we are dealing with transmissions that were received on the local device, if {@code false} we are dealing with transmissions created for sending from the local device to other ones
	 * @return 
	 */
	private ForeignKeyColumn getResponseColumn(boolean incoming)
	{
		return (ForeignKeyColumn) getTransmissionSchema(incoming).getColumn(TRANSMISSION_COLUMN_NAME_RESPONSE, false);
	}
	
	public synchronized void store(Transmission<?> transmission) throws DBException
	{
		// Start transaction
		recordStore.startTransaction();
		
		try
		{
			// Use TransmissionRecordGenerator to create a transmission record and part record(s):
			List<Record> records = generator.generate(transmission);
			if(records.size() < 2)
				throw new IllegalStateException("No transmission (part) record(s) generated!");
			Record tRec = records.get(0);
			
			// Set foreign key for Correspondent record (possibly first storing/updating it):
			TRANSMISSION_COLUMN_CORRESPONDENT.storeValue(tRec, getCorrespondentRecordReference(transmission.getCorrespondent(), true, false));
			
			// Store the transmission record:
			recordStore.store(tRec);
			//	local ID should now be set in the record...
			
			// Check/set it on the object:
			if(transmission.isLocalIDSet()) // if the object already had a local transmissionID...
			{	// then it should match the ID on the record, so let's verify:
				if(transmission.getLocalID() != TRANSMISSION_COLUMN_ID.retrieveValue(tRec).intValue())
					throw new IllegalStateException("Non-matching transmission ID"); // this should never happen
			}
			else
				// Set local transmissionID in object as on the record: 
				transmission.setLocalID(TRANSMISSION_COLUMN_ID.retrieveValue(tRec).intValue());
			
			// Store part records:
			ForeignKeyColumn tFKCol = transmission.incoming ? TRANSMISSION_PART_COLUMN_INCOMING_TRANSMISSION : TRANSMISSION_PART_COLUMN_OUTGOING_TRANSMISSION;
			RecordReference tRecRef = tRec.getReference();
			for(Record tPartRec : records.subList(1, records.size()))
			{
				tFKCol.storeValue(tPartRec, tRecRef); // set foreign key!
				recordStore.store(tPartRec);
			}
			
			// Put/update in cache:
			getCache(transmission.incoming).put(transmission.getLocalID(), transmission);
		}
		catch(Exception e)
		{
			recordStore.rollbackTransactions();
			if(e instanceof DBException)
				throw (DBException) e;
			throw new DBException(e);
		}
		
		// Commit transaction
		recordStore.commitTransaction();
	}
	
	/**
	 * @param incoming if {@code true} the transmission was received on the local device, if {@code false} it was created for sending from the local device to another one
	 * @param type
	 * @param localID
	 * @param remoteID
	 * @param correspondent
	 * @param payloadHash
	 * @param numberOfParts
	 * @return
	 * @throws UnknownCorrespondentException 
	 */
	private RecordsQuery getTransmissionsQuery(boolean incoming, Transmission.Type type, Integer localID, Integer remoteID, Correspondent correspondent, Integer payloadHash, Integer numberOfParts) throws UnknownCorrespondentException
	{
		if(correspondent != null && !correspondent.isLocalIDSet())
			throw new UnknownCorrespondentException("Correspondent (" + correspondent.toString() + ") is unknown in database.");
		return new RecordsQuery(
			// schema (sent/received):
			getTransmissionSchema(incoming),
			// localID:
			(localID != null ? getTransmissionSchema(incoming).createRecordReference(localID).getRecordQueryConstraint() : null),
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
	 * @throws IllegalStateException when more than 1 matching Transmission is found (cannot happen for queries that check localID, which is unique)
	 */
	protected Transmission<?> retrieveTransmissionByQuery(RecordsQuery multiRecordQuery, boolean knownLocalID) throws IllegalStateException
	{
		List<Transmission<?>> results = retrieveTransmissionsByQuery(multiRecordQuery);
		if(results.size() > 1)
		{
			if(!knownLocalID)
				throw new IllegalStateException("Found more than 1 matching transmission for query");
			else
				return null; // this should never happen
		}
		if(results.isEmpty())
			return null;
		else
			return results.get(0);
	}
	
	protected List<Transmission<?>> retrieveTransmissionsByQuery(RecordsQuery multiRecordQuery)
	{
		List<Transmission<?>> transmissions = new ArrayList<Transmission<?>>();
		for(Record record : recordStore.retrieveRecords(multiRecordQuery))
			CollectionUtils.addIgnoreNull(transmissions, transmissionFromRecord(record, false)); // convert to Transmission objects (skipping "hidden" deleted transmissions)
		return transmissions;
	}
	
	private Transmission<?> transmissionFromRecord(Record tRec, boolean includeDeleted)
	{
		// Null check:
		if(tRec == null)
			return null; // no such transmission found
		
		// Essential values:
		boolean incoming = tRec.getSchema().equals(INCOMING_TRANSMISSION_SCHEMA);
		int localID = TRANSMISSION_COLUMN_ID.retrieveValue(tRec).intValue();
		
		// Check cache:
		if(getCache(incoming).containsKey(localID))
			return getCache(incoming).get(localID);
		
		// Check if transmission is not deleted by hiding:
		if(TRANSMISSION_COLUMN_DELETED.retrieveValue(tRec) && !includeDeleted)
			return null;
		
		// Other values:
		Transmission.Type type = Transmission.Type.values()[TRANSMISSION_COLUMN_TYPE.retrieveValue(tRec).intValue()]; 
		Integer remoteID = TRANSMISSION_COLUMN_REMOTE_ID.isValuePresent(tRec) ? TRANSMISSION_COLUMN_REMOTE_ID.retrieveValue(tRec).intValue() : null;
		Integer payloadType = TRANSMISSION_COLUMN_PAYLOAD_TYPE.isValuePresent(tRec) ? TRANSMISSION_COLUMN_PAYLOAD_TYPE.retrieveValue(tRec).intValue() : null;
		int payloadHash = TRANSMISSION_COLUMN_PAYLOAD_HASH.retrieveValue(tRec).intValue();
		TimeStamp sentAt = retrieveTimeStamp(COLUMN_SENT_AT, tRec);
		TimeStamp receivedAt = retrieveTimeStamp(COLUMN_RECEIVED_AT, tRec);
		int totalParts = TRANSMISSION_COLUMN_NUMBER_OF_PARTS.retrieveValue(tRec).intValue();
		//	Columns only occurring on receiving side:
		int numberOfSentResendRequests = incoming ? TRANSMISSION_COLUMN_NUMBER_OF_RESEND_REQS_SENT.retrieveValue(tRec).intValue() : 0;
		TimeStamp lastResendReqSentAt =	incoming ? retrieveTimeStamp(TRANSMISSION_COLUMN_LAST_RESEND_REQS_SENT_AT, tRec) : null;
		RecordReference responseRecRef = getResponseColumn(incoming).retrieveValue(tRec);
		Transmission<?> response = responseRecRef != null ? retrieveTransmission(!incoming, TRANSMISSION_COLUMN_ID.retrieveValue(responseRecRef).intValue()) : null;
				
		// Query for correspondent record:
		Record cRec = TRANSMISSION_COLUMN_CORRESPONDENT.isValuePresent(tRec) ? recordStore.retrieveRecord(TRANSMISSION_COLUMN_CORRESPONDENT.retrieveValue(tRec)) : null;
		
		// Query for part records:		
		List<Record> tPartRecs = recordStore.retrieveRecords(new RecordsQuery(Source.From(getTransmissionPartSchema(incoming)), Order.AscendingBy(TRANSMISSION_PART_COLUMN_NUMBER), tRec.getRecordQueryConstraint()));
		
		// Instantiate Transmissions & Messages:
		switch(type)
		{
			case GeoKey:
				return new GeoKeyTransmission(client, this.<GeoKeyServer> correspondentFromRecord(cRec), incoming, localID, remoteID, payloadType, payloadHash, lastResendReqSentAt, receivedAt, (GeoKeyTransmission) response, TRANSMISSION_PART_COLUMN_BODY.retrieveValue(tPartRecs.get(0)));
			default:
				throw new IllegalStateException("Unsupported transmission type");
		}
	}
	
	/**
	 * Retrieve an incoming or outgoing transmission by its local ID.
	 * 
	 * @param incoming if {@code true} the transmission was received on the local device, if {@code false} it was created for sending from the local device to another one
	 * @param localID
	 * 
	 * @return the Transmission with the given {@code localID}, or {@code null} if no such transmission was found.
	 */
	public synchronized Transmission<?> retrieveTransmission(boolean incoming, int localID)
	{
		return retrieveTransmission(incoming, localID, false);
	}
	
	/**
	 * Retrieve an incoming or outgoing transmission by its local ID.
	 * 
	 * @param incoming if {@code true} the transmission was received on the local device, if {@code false} it was created for sending from the local device to another one
	 * @param localID
	 * @param findDeleted whether or not to retrieve transmission which are deleted by hiding
	 * 
	 * @return the Transmission with the given {@code localID}, or {@code null} if no such transmission was found.
	 */
	public synchronized Transmission<?> retrieveTransmission(boolean incoming, int localID, boolean findDeleted)
	{
		try
		{
			// Check cache:
			if(getCache(incoming).containsKey(localID))
				return getCache(incoming).get(localID);
			//else:
			return transmissionFromRecord(recordStore.retrieveRecord(getTransmissionSchema(incoming).createRecordReference(localID)), findDeleted);
		}
		catch(Exception e)
		{
			client.logError("Error retrieving " + (incoming ? "received" : "sent") + " transmission with local ID = " + localID + ".", e);
			return null;
		}
	}
	
	/**
	 * Retrieve an incoming or outgoing transmission by a RecordReference pointing to a Transmission record.
	 * 
	 * @param transmissionRecordReference
	 * @param findDeleted whether or not to retrieve transmission which are deleted by hiding
	 * @return
	 */
	protected synchronized Transmission<?> retrieveTransmission(RecordReference transmissionRecordReference, boolean findDeleted)
	{
		return retrieveTransmission(transmissionRecordReference.getReferencedSchema() == INCOMING_TRANSMISSION_SCHEMA,
									TRANSMISSION_COLUMN_ID.retrieveValue(transmissionRecordReference).intValue());
	}
	
	/**
	 * @param incoming if {@code true} the transmission was received on the local device, if {@code false} it was created for sending from the local device to another one
	 * @param localID
	 * @param payloadHash
	 * @return a matching Transmission, or {@code null} if no such transmission was found or an error occurred (check log output).
	 */
	public synchronized Transmission<?> retrieveTransmission(boolean incoming, int localID, int payloadHash)
	{
		return retrieveTransmission(incoming, localID, payloadHash, null);
	}
	
	/**
	 * @param incoming if {@code true} the transmission was received on the local device, if {@code false} it was created for sending from the local device to another one
	 * @param localID
	 * @param payloadHash
	 * @param numberOfParts
	 * @return a matching Transmission, or {@code null} if no such transmission was found or an error occurred (check log output).
	 */
	public Transmission<?> retrieveTransmission(boolean incoming, int localID, int payloadHash, Integer numberOfParts)
	{
		return retrieveTransmissionByQuery(getTransmissionsQuery(incoming, null, localID, null, null, payloadHash, numberOfParts), true);
	}
	
	/**
	 * Retrieve an incoming or outgoing transmission by its type (binary/textual), correspondent, remote(!) ID, payload hash, and number of parts.
	 * 
	 * @param incoming
	 * @param type
	 * @param correspondent
	 * @param remoteID - not local!
	 * @param payloadHash
	 * @param numberOfParts
	 * @return a matching Transmission, or {@code null} if no such transmission was found
	 * @throws IllegalStateException when more than 1 matching Transmission is found
	 * @throws UnknownCorrespondentException when the correspondent is unknown
	 */
	public Transmission<?> retrieveTransmission(boolean incoming, Transmission.Type type, Correspondent correspondent, int remoteID, int payloadHash, int numberOfParts) throws IllegalStateException, UnknownCorrespondentException
	{
		return retrieveTransmissionByQuery(getTransmissionsQuery(incoming, type, null, remoteID, correspondent, payloadHash, numberOfParts), false);
	}
	
	/**
	 * Returns a list of all incoming or outgoing transmissions from or to the given correspondent.
	 * 
	 * @param incoming
	 * @param correspondent
	 * @return
	 */
	public List<Transmission<?>> retrieveTransmissions(boolean incoming, Correspondent correspondent)
	{
		try
		{
			return retrieveTransmissionsByQuery(getTransmissionsQuery(incoming, null, null, null, correspondent, null, null));
		}
		catch(UnknownCorrespondentException uce)
		{
			return Collections.<Transmission<?>> emptyList();
		}
	}

	/**
	 * @param transmission
	 * @param byHiding if {@code true} the transmission will only be hidden (marked as deleted, but still in the db), if {@code false} it (and its parts) will be completely removed from the db
	 */
	public void deleteTransmission(Transmission<?> transmission, boolean byHiding)
	{
		if(!transmission.isLocalIDSet())
			return; // the transmission was never stored
		try
		{
			recordStore.startTransaction();
			
			// Get record reference:
			RecordReference tRecRef = getTransmissionSchema(transmission.incoming).createRecordReference(transmission.getLocalID());
			
			if(byHiding)
			{
				Record tRec = generator.generate(transmission).get(0);
				TRANSMISSION_COLUMN_DELETED.storeValue(tRec, Boolean.TRUE);
				// Store the transmission record:
				recordStore.store(tRec);
			}
			else
			{	// Really delete from db:
				//	Delete transmission part records:
				recordStore.delete(new RecordsQuery(Source.From(getTransmissionPartSchema(transmission.incoming)), tRecRef.getRecordQueryConstraint()));
				
				//	Delete transmission record:
				recordStore.delete(tRecRef);
			}
			
			recordStore.commitTransaction();
			
			// Delete from cache:
			getCache(transmission.incoming).remove(transmission.getLocalID());
		}
		catch(Exception e)
		{
			client.logError("Error upon deleting translission (local ID: " + transmission.getLocalID() + ")", e);
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
				transmission != null ? getTransmissionSchema(false).createRecordReference(transmission.getLocalID()) : null,
				// Received column:
				Boolean.valueOf(transmission != null && transmission.isReceived())));
		}
		catch(Exception e)
		{
			client.logError("Error upon storing transmittable", e);
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
			client.logError("Error upon deleting transmittable(s)", e);
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
	 * @param model
	 * @param transmission
	 * @return
	 */
	public List<Record> retrieveTransmittableRecordsWithTransmission(Model model, Transmission<?> transmission)
	{
		return retrieveTransmittableUserRecords(transmission.getCorrespondent(), model, getTransmissionSchema(false).createRecordReference(transmission.getLocalID()).getRecordQueryConstraint());
	}
	
	/**
	 * @param receiver
	 * @param model
	 * @return
	 */
	public synchronized List<Record> retrieveRecordsToTransmitNow(Correspondent receiver, Model model)
	{
		List<Record> recsToSend = new ArrayList<Record>();
		
		// Query for unsent (as in, not associated with a transmission) records for the given receiver & model:
		CollectionUtils.addAllIgnoreNull(recsToSend, retrieveTransmittableRecordsWithoutTransmission(receiver, model));
		
		//Also include transmittable records which have a transmission which was never sent or for which we haven't received a response since the timeout:
		CollectionUtils.addAllIgnoreNull(recsToSend, retrieveTransmittableRecordsForResending(receiver, model));
		
		return recsToSend;
	}
	
	/**
	 * Gets all unreceived {@link #TRANSMITTABLE_RECORDS_SCHEMA} records with an assigned transmission, grouped by transmission.
	 * 
	 * @param correspondent
	 * @param model
	 * @return Map<RecordReference, List<Record>>: key = transmission record reference; value = list of associated {@link #TRANSMITTABLE_RECORDS_SCHEMA} records
	 */
	private synchronized Map<RecordReference, List<Record>> retrieveUnreceivedTransmittablesWithTransmission(Correspondent correspondent, Model model)
	{
		List<Record> toSendRecs = retrieveTransmittableRecords(
			correspondent, model,
			Order.By(TRANSMITTABLE_RECORDS_COLUMN_TRANSMISSION), // order by the transmission
			new AndConstraint(	// with transmission reference:
								EqualityConstraint.IsNotNull(TRANSMITTABLE_RECORDS_COLUMN_TRANSMISSION),
								// with Received=false:
								new EqualityConstraint(TRANSMITTABLE_RECORDS_COLUMN_RECEIVED, Boolean.FALSE)));
		
		// Group by transmission record reference:
		Map<RecordReference, List<Record>> tRecRef2toSendRecs = new HashMap<RecordReference, List<Record>>();
		RecordReference prevTRecRef = null;
		for(Record toSendRec : toSendRecs)
		{
			RecordReference tRefRec = TRANSMITTABLE_RECORDS_COLUMN_TRANSMISSION.retrieveValue(toSendRec);
			if(!Objects.equals(prevTRecRef, tRefRec))
			{
				tRecRef2toSendRecs.put(tRefRec, new ArrayList<Record>());
				prevTRecRef = tRefRec;
			}
			tRecRef2toSendRecs.get(tRefRec).add(toSendRec);
		}
		
		// Return map:
		return tRecRef2toSendRecs;
	}
	
	/**
	 * @param correspondent
	 * @param model
	 * @return
	 */
	public synchronized List<Record> retrieveTransmittableRecordsForResending(Correspondent correspondent, Model model)
	{
		// Get all unreceived transmittables with an assigned transmission:
		Map<RecordReference, List<Record>> tRecRef2toSendRecs =  retrieveUnreceivedTransmittablesWithTransmission(correspondent, model);
				
		// Collection to return:
		List<Record> userRecs = new ArrayList<Record>();
		
		// Treat per transmission:
		for(Map.Entry<RecordReference, List<Record>> entry : tRecRef2toSendRecs.entrySet())
		{
			// Get transmission object:
			Transmission<?> transmission = retrieveTransmission(entry.getKey(), false /*don't include deleted*/);

			if(	// unknown/deleted transmission:
				transmission == null ||
				// transmission is not received "says" it is appropriate to have its contents resent now:
				(!transmission.isReceived() && transmission.isResendAppropriate()))
			{
				// Get user records for resending:
				for(Record toSendRec : entry.getValue())
					CollectionUtils.addIgnoreNull(userRecs, getUserRecordFromTransmittable(toSendRec, model));
				// Delete transmission if there was one:
				if(transmission != null)
					deleteTransmission(transmission, true /*deleting by hiding*/);
			}
			else if(transmission.isReceived())
			{	// transmission is received (i.e. ACKed):
				for(Record toSendRec : entry.getValue())
					try
					{	// Mark transmittable as received:
						TRANSMITTABLE_RECORDS_COLUMN_RECEIVED.storeValue(toSendRec, Boolean.TRUE);
						recordStore.store(toSendRec);
					}
					catch(Exception e)
					{
						client.logError("Error upon storing transmittable", e);
					}
			}
		}
		
		return userRecs;
	}
	
	public synchronized void updateTransmittableReceivedState(Correspondent correspondent, Model model)
	{
		// Get all unreceived transmittables with an assigned transmission:
		Map<RecordReference, List<Record>> tRecRef2toSendRecs =  retrieveUnreceivedTransmittablesWithTransmission(correspondent, model);
		
		// Treat per transmission:
		for(Map.Entry<RecordReference, List<Record>> entry : tRecRef2toSendRecs.entrySet())
		{
			Transmission<?> transmission = retrieveTransmission(entry.getKey(), false /*don't include deleted*/);
			if(transmission != null && transmission.isReceived())
			{	// transmission is received (i.e. ACKed):
				for(Record toSendRec : entry.getValue())
					try
					{	// Mark transmittable as received:
						TRANSMITTABLE_RECORDS_COLUMN_RECEIVED.storeValue(toSendRec, Boolean.TRUE);
						recordStore.store(toSendRec);
					}
					catch(Exception e)
					{
						client.logError("Error upon storing transmittable", e);
					}
			}
		}
	}
	
	/**
	 * @param correspondent
	 * @param model
	 * @return RecordReferences pointing to the Records of the given Model received by the given Correspondent
	 */
	public synchronized List<RecordReference> retrieveReceivedRecords(Correspondent correspondent, Model model)
	{
		// Updated received flags:
		updateTransmittableReceivedState(correspondent, model);
		
		// Get all transmittables with received=true:
		List<Record> toSendRecs = retrieveTransmittableRecords(
				correspondent, model,
				Order.UNDEFINED,
				new EqualityConstraint(TRANSMITTABLE_RECORDS_COLUMN_RECEIVED, Boolean.TRUE));
		
		// Get and return user record references:
		List<RecordReference> userRecRefs = new ArrayList<RecordReference>(toSendRecs.size());
		for(Record toSendRec : toSendRecs)
			CollectionUtils.addIgnoreNull(userRecRefs, getUserRecordReferenceFromTransmittable(toSendRec, model));
		return userRecRefs;
	}
	
	/**
	 * @param correspondent
	 * @param model
	 * @param constraint - may be null
	 * @return a possibly empty list of {@link #TRANSMITTABLE_RECORDS_SCHEMA} records
	 */
	private List<Record> retrieveTransmittableRecords(Correspondent correspondent, Model model, Order order, Constraint constraint)
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
								constraint));
		
		// Return result:
		return toSendRecs;
	}
	
	/**
	 * @param correspondent
	 * @param model
	 * @param contraint - may be null
	 * @return
	 */
	private List<Record> retrieveTransmittableUserRecords(Correspondent correspondent, Model model, Constraint contraint)
	{
		// Query for ToSend records:
		List<Record> toSendRecs = retrieveTransmittableRecords(correspondent, model, Order.By(TRANSMITTABLE_RECORDS_CP_SCHEMA_NUMBER), contraint);
		
		// Query for the actual records being referred to:
		List<Record> userRecs = new ArrayList<Record>(toSendRecs.size());
		for(Record toSendRec : toSendRecs)
			CollectionUtils.addIgnoreNull(userRecs, getUserRecordFromTransmittable(toSendRec, model));
		
		// Return result:
		return userRecs;
	}
	
	private RecordReference getUserRecordReferenceFromTransmittable(Record toSendRecord, Model recycleModel)
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
			// Return reference to user record:
			return schema.createRecordReference(TRANSMITTABLE_RECORDS_COLUMN_PK_VALUES.retrieveValue(toSendRecord));	
		}
		catch(Exception e)
		{
			client.logError("Failed to retrieve user record reference for transmittable entry: " + toSendRecord.toString(false), e);
			return null;
		}
	}
	
	private Record getUserRecordFromTransmittable(Record toSendRecord, Model recycleModel)
	{
		// Query for & return user record:
		return recordStore.retrieveRecord(getUserRecordReferenceFromTransmittable(toSendRecord, recycleModel));
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
		public void handle(GeoKeyServer geokeyAccount)
		{
			// does nothing (for now)
		}
		
	}
	
	/**
	 * Helper class to generate Records representing Transmissions and their parts (Messages)
	 * 
	 * @author mstevens
	 */
	private class TransmissionRecordGenerator implements Transmission.Handler
	{

		private Record tRecord;
		private final List<Record> tPartRecords = new ArrayList<Record>();
		
		/**
		 * @param transmission
		 * @return a {@link List} of {@link Record}s, the first one of which is the tranmission record, the following ones are the transmission part records
		 */
		public List<Record> generate(Transmission<?> transmission)
		{	
			// Create new transmission record:
			tRecord = getTransmissionSchema(transmission.incoming).createRecord();
			
			// wipe part recs:
			tPartRecords.clear();
			
			// Set values of all columns will be set except for Correspondent & NumberOfParts:
			if(transmission.isLocalIDSet())
				TRANSMISSION_COLUMN_ID.storeValue(tRecord, transmission.getLocalID());	
			if(transmission.isRemoteIDSet())
				TRANSMISSION_COLUMN_REMOTE_ID.storeValue(tRecord, transmission.getRemoteID());
			TRANSMISSION_COLUMN_TYPE.storeValue(tRecord, transmission.getType().ordinal());
			TRANSMISSION_COLUMN_PAYLOAD_HASH.storeValue(tRecord, transmission.getPayloadHash()); // payload hash should always be set before storage
			if(transmission.isPayloadTypeSet())
				TRANSMISSION_COLUMN_PAYLOAD_TYPE.storeValue(tRecord, transmission.getPayloadType());
			if(transmission.isSent())
				COLUMN_SENT_AT.storeValue(tRecord, transmission.getSentAt());
			if(transmission.isReceived())
				COLUMN_RECEIVED_AT.storeValue(tRecord, transmission.getReceivedAt());
			getResponseColumn(transmission.incoming).storeValue(
				tRecord,
				transmission.hasResponse() && transmission.getResponse().isLocalIDSet() ?
					getTransmissionSchema(!transmission.incoming).createRecordReference(transmission.getResponse().getLocalID()) :
					null);
			
			// Use double dispatch for type-specific work:
			transmission.handle(this);
			
			// Return list with tRecord and tPartRecords:
			List<Record> result = new ArrayList<Record>(tPartRecords.size() + 1);
			result.add(tRecord);
			result.addAll(tPartRecords);
			return result;
		}
		
		private Record newPartRecord(Transmission<?> transmission, int partNumber)
		{
			Record tPartRec = getTransmissionPartSchema(transmission.incoming).createRecord();
			tPartRecords.add(tPartRec);
			TRANSMISSION_PART_COLUMN_NUMBER.storeValue(tPartRec, partNumber);
			return tPartRec;
		}

		@Override
		public void handle(GeoKeyTransmission geoKeyT)
		{
			// Set number of parts (always = 1):
			TRANSMISSION_COLUMN_NUMBER_OF_PARTS.storeValue(tRecord, 1);
			if(geoKeyT.incoming) // columns only occuring on receiving side
			{
				// Set number of resend requests (always = 0):
				TRANSMISSION_COLUMN_NUMBER_OF_RESEND_REQS_SENT.storeValue(tRecord, 0);
				// TRANSMISSION_COLUMN_LAST_RESEND_REQS_SENT_AT remains null
			}
			
			// Create a single transmission part (only used to store the body):
			newPartRecord(geoKeyT, 1); // adds to the list as well
			setPartBody(geoKeyT.getBody()); // will set part body and body bit length
		}
		
		private void setPartBody(byte[] bodyBytes)
		{
			setPartBody(bodyBytes, bodyBytes.length * Byte.SIZE);
		}
		
		private void setPartBody(byte[] bodyBytes, int bitLength)
		{
			// Last tPartRec in the list:
			Record tPartRec = tPartRecords.get(tPartRecords.size() - 1);
			
			// Set body & body bit length columns:
			TRANSMISSION_PART_COLUMN_BODY.storeValue(tPartRec, bodyBytes);
			TRANSMISSION_PART_COLUMN_BODY_BIT_LENGTH.storeValue(tPartRec, bitLength);
		}

	}
	
}
