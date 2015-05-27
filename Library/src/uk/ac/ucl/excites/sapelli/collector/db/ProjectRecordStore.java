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

package uk.ac.ucl.excites.sapelli.collector.db;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import uk.ac.ucl.excites.sapelli.collector.CollectorClient;
import uk.ac.ucl.excites.sapelli.collector.db.exceptions.ProjectIdentificationClashException;
import uk.ac.ucl.excites.sapelli.collector.db.exceptions.ProjectSignatureClashException;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.load.FormSchemaInfoProvider;
import uk.ac.ucl.excites.sapelli.collector.load.ProjectLoader;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Relationship;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendingSchedule;
import uk.ac.ucl.excites.sapelli.shared.db.StoreBackupper;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBConstraintException;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBPrimaryKeyException;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.ListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.RecordReference;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.storage.model.indexes.AutoIncrementingPrimaryKey;
import uk.ac.ucl.excites.sapelli.storage.model.indexes.Index;
import uk.ac.ucl.excites.sapelli.storage.model.indexes.PrimaryKey;
import uk.ac.ucl.excites.sapelli.storage.queries.FirstRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Order;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Source;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.EqualityConstraint;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;

/**
 * A RecordStore based implementation of ProjectStore
 * 
 * @author mstevens
 */
public class ProjectRecordStore extends ProjectStore implements StoreHandle.StoreUser, FormSchemaInfoProvider
{
	
	// STATICS---------------------------------------------
	// Project storage model:
	//	Model:
	static public final Model COLLECTOR_MANAGEMENT_MODEL = new Model(CollectorClient.COLLECTOR_MANAGEMENT_MODEL_ID, "CollectorManagement");
	//	 Project schema:
	static public final Schema PROJECT_SCHEMA = new Schema(COLLECTOR_MANAGEMENT_MODEL, "Project");
	//		Columns:
	static private final IntegerColumn PROJECT_ID_COLUMN = PROJECT_SCHEMA.addColumn(new IntegerColumn("id", false, Project.PROJECT_ID_FIELD));
	static private final IntegerColumn PROJECT_FINGERPRINT_COLUMN = PROJECT_SCHEMA.addColumn(new IntegerColumn("fingerPrint", false, true, Project.PROJECT_FINGERPRINT_SIZE));
	static private final StringColumn PROJECT_NAME_COLUMN = PROJECT_SCHEMA.addColumn(StringColumn.ForCharacterCount("name", false, 128));
	/*			Note on variant column:
	 * 				Even though project#variant can be null we make the column non-optional (and will store nulls as ""),
	 * 				the reason is that this allows us to enforce a unique index on (name, variant, version), to avoid
	 * 				projects with duplicate signature. Such an index would not behave in this desired way when one of the
	 * 				columns in nullable, as per the SQL standard and common implementations thereof (see http://www.sqlite.org/faq.html#q26) */
	static private final StringColumn PROJECT_VARIANT_COLUMN = PROJECT_SCHEMA.addColumn(StringColumn.ForCharacterCount("variant", false /*see comment*/, 128));
	static private final StringColumn PROJECT_VERSION_COLUMN = PROJECT_SCHEMA.addColumn(StringColumn.ForCharacterCount("version", false, 32));
	static private final IntegerColumn PROJECT_V1X_SCHEMA_VERSION_COLUMN = PROJECT_SCHEMA.addColumn(new IntegerColumn("v1xSchemaVersion", true, Schema.V1X_SCHEMA_VERSION_FIELD));
	//		Add index, set primary key & seal schema:
	static
	{
		// Unique index to ensure name+variant+version combinations are unique:
		PROJECT_SCHEMA.addIndex(new Index("ProjectUnique", true, PROJECT_NAME_COLUMN, PROJECT_VARIANT_COLUMN, PROJECT_VERSION_COLUMN));
		PROJECT_SCHEMA.setPrimaryKey(PrimaryKey.WithColumnNames(PROJECT_ID_COLUMN, PROJECT_FINGERPRINT_COLUMN));
		PROJECT_SCHEMA.seal(); // !!!
	}
	//	 Form Schema Info (FSI) schema:
	static public final Schema FSI_SCHEMA = new Schema(COLLECTOR_MANAGEMENT_MODEL, "FormSchemaInfo");
	//		Columns:
	static private final ForeignKeyColumn FSI_PROJECT_KEY_COLUMN = FSI_SCHEMA.addColumn(new ForeignKeyColumn(PROJECT_SCHEMA, false));
	static private final IntegerColumn FSI_FORM_POSITION_COLUMN = FSI_SCHEMA.addColumn(new IntegerColumn("formPosition", false, 0, Project.MAX_FORMS - 1));
	static private final ListColumn.Simple<String> FSI_BYPASSABLE_FIELD_IDS_COLUMN = FSI_SCHEMA.addColumn(new ListColumn.Simple<String>("byPassableFieldIDs", StringColumn.ForCharacterCount("FieldID", false, Field.MAX_ID_LENGTH), true, Form.MAX_FIELDS));
	//		Set primary key & seal schema:
	static
	{
		FSI_SCHEMA.setPrimaryKey(PrimaryKey.WithColumnNames(FSI_PROJECT_KEY_COLUMN, FSI_FORM_POSITION_COLUMN));
		FSI_SCHEMA.seal(); // !!!
	}
	//	 Held Foreign Key (HFK) schema: to store "held" foreign keys (RecordReferences) on Relationship fields
	static public final Schema HFK_SCHEMA = new Schema(COLLECTOR_MANAGEMENT_MODEL, "HeldForeignKey");
	//		Columns:
	static private final ForeignKeyColumn HFK_PROJECT_KEY_COLUMN = HFK_SCHEMA.addColumn(new ForeignKeyColumn(PROJECT_SCHEMA, false));
	static private final IntegerColumn HFK_FORM_POSITION_COLUMN = HFK_SCHEMA.addColumn(new IntegerColumn("formPosition", false, 0, Project.MAX_FORMS - 1));
	static private final IntegerColumn HFK_RELATIONSHIP_FIELD_POSITION_COLUMN = HFK_SCHEMA.addColumn(new IntegerColumn("relationshipFieldPosition", false, 0, Form.MAX_FIELDS - 1));
	static private final StringColumn HFK_SERIALISED_RECORD_REFERENCE = HFK_SCHEMA.addColumn(StringColumn.ForCharacterCount("serialisedRecordReference", false, 256));
	//		Set primary key & seal schema:
	static
	{
		HFK_SCHEMA.setPrimaryKey(PrimaryKey.WithColumnNames(HFK_PROJECT_KEY_COLUMN, HFK_FORM_POSITION_COLUMN, HFK_RELATIONSHIP_FIELD_POSITION_COLUMN));
		HFK_SCHEMA.seal(); // !!!
	}
	
	// Record-sending schedule Schema
	static final public Schema SEND_RECORDS_SCHEDULE_SCHEMA = new Schema(COLLECTOR_MANAGEMENT_MODEL, "SendRecordsSchedule");
	static final public IntegerColumn SEND_RECORDS_SCHEDULE_COLUMN_ID = new IntegerColumn("ID", false);
	static final public ForeignKeyColumn SEND_RECORDS_SCHEDULE_COLUMN_PROJECT = new ForeignKeyColumn("Project", ProjectRecordStore.PROJECT_SCHEMA, false);
	static final public ForeignKeyColumn SEND_RECORDS_SCHEDULE_COLUMN_RECEIVER = new ForeignKeyColumn("Receiver", TransmissionStore.RECEIVER_SCHEMA, false);
	static final public IntegerColumn SEND_RECORDS_SCHEDULE_COLUMN_INTERVAL = new IntegerColumn("RetransmitInterval", false, false); // unsigned 32 bits
	static final public BooleanColumn SEND_RECORDS_SCHEDULE_COLUMN_ENCRYPT = new BooleanColumn("Encrypt", false);
	//	Add columns to Receiver Schema & seal it:
	static
	{
		SEND_RECORDS_SCHEDULE_SCHEMA.addColumn(SEND_RECORDS_SCHEDULE_COLUMN_ID);
		SEND_RECORDS_SCHEDULE_SCHEMA.addColumn(SEND_RECORDS_SCHEDULE_COLUMN_PROJECT);
		SEND_RECORDS_SCHEDULE_SCHEMA.addColumn(SEND_RECORDS_SCHEDULE_COLUMN_RECEIVER);
		SEND_RECORDS_SCHEDULE_SCHEMA.addColumn(SEND_RECORDS_SCHEDULE_COLUMN_INTERVAL);
		SEND_RECORDS_SCHEDULE_SCHEMA.addColumn(SEND_RECORDS_SCHEDULE_COLUMN_ENCRYPT);
		SEND_RECORDS_SCHEDULE_SCHEMA.setPrimaryKey(new AutoIncrementingPrimaryKey("IDIdx", SEND_RECORDS_SCHEDULE_COLUMN_ID));
		SEND_RECORDS_SCHEDULE_SCHEMA.seal();
	}
	
	// "Record receival" (incoming records) Schema -- may be used at some point in the future to keep track of sending correspondents
//	static final public Schema RECORD_RECEIVAL_SCHEMA = new Schema(COLLECTOR_MANAGEMENT_MODEL, "RecordReceival");
//	static final public IntegerColumn RECORD_RECEIVAL_COLUMN_ID = new IntegerColumn("ID", false, RecordReceival.SENDER_ID_FIELD);
//	static final public ForeignKeyColumn RECORD_RECEIVAL_COLUMN_PROJECT_ID = new ForeignKeyColumn("ProjectID", ProjectRecordStore.PROJECT_SCHEMA, false);
//	static final public ForeignKeyColumn RECORD_RECEIVAL_COLUMN_SENDER_ID = new ForeignKeyColumn("SenderID", TransmissionStore.SENDER_SCHEMA, false);
//	static final public BooleanColumn RECORD_RECEIVAL_COLUMN_ACK = new BooleanColumn("Ack", false);
//	// Add columns to Sender Schema and seal it:
//	static
//	{
//		RECORD_RECEIVAL_SCHEMA.addColumn(RECORD_RECEIVAL_COLUMN_ID);
//		RECORD_RECEIVAL_SCHEMA.addColumn(RECORD_RECEIVAL_COLUMN_PROJECT_ID);
//		RECORD_RECEIVAL_SCHEMA.addColumn(RECORD_RECEIVAL_COLUMN_SENDER_ID);
//		RECORD_RECEIVAL_SCHEMA.addColumn(RECORD_RECEIVAL_COLUMN_ACK);
//		RECORD_RECEIVAL_SCHEMA.setPrimaryKey(new AutoIncrementingPrimaryKey("IDIdx", RECORD_RECEIVAL_COLUMN_ID));
//		RECORD_RECEIVAL_SCHEMA.seal();
//	}
	
	// Seal the model itself:
	static
	{
		COLLECTOR_MANAGEMENT_MODEL.seal();
	}
			
	// DYNAMICS--------------------------------------------
	private final CollectorClient client;
	private final RecordStore recordStore;
	private final FileStorageProvider fileStorageProvider;
	private final Map<Long, Project> cache;
	
	/**
	 * @param client
	 * @param fileStorageProvider
	 * @throws DBException
	 */
	public ProjectRecordStore(CollectorClient client, FileStorageProvider fileStorageProvider) throws DBException
	{
		this.client = client;
		this.recordStore = client.recordStoreHandle.getStore(this);
		this.fileStorageProvider = fileStorageProvider;
		this.cache = new HashMap<Long, Project>();
	}
	
	private Record getProjectRecord(Project project)
	{
		Record projRec = PROJECT_SCHEMA.createRecord();
		PROJECT_ID_COLUMN.storeValue(projRec, project.getID());
		PROJECT_FINGERPRINT_COLUMN.storeValue(projRec, project.getFingerPrint());
		PROJECT_NAME_COLUMN.storeValue(projRec, project.getName());
		String projVariant = project.getVariant();
		PROJECT_VARIANT_COLUMN.storeValue(projRec, projVariant != null ? projVariant : ""); // replace null by empty string!
		PROJECT_VERSION_COLUMN.storeValue(projRec, project.getVersion());
		PROJECT_V1X_SCHEMA_VERSION_COLUMN.storeValue(projRec, project.getV1XSchemaVersion());
		return projRec;
	}
	
	private RecordReference getProjectRecordReference(Project project)
	{
		return PROJECT_SCHEMA.createRecordReference(project.getID(),
													project.getFingerPrint());
	}
	
	private Record getFSIRecord(Form form)
	{
		return FSI_SCHEMA.createRecord(	getProjectRecordReference(form.project),
										form.getPosition(),
										form.getColumnOptionalityAdvisor().getIDsOfByPassableNonOptionalFieldsWithColumn());
	}
	
	private RecordReference getFSIRecordReference(Form form)
	{
		return FSI_SCHEMA.createRecordReference(getProjectRecordReference(form.project),
												form.getPosition());
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.load.FormSchemaInfoProvider#getByPassableFieldIDs(uk.ac.ucl.excites.sapelli.collector.model.Form)
	 */
	@Override
	public List<String> getByPassableFieldIDs(Form form)
	{
		Record fsiRec = recordStore.retrieveRecord(getFSIRecordReference(form).getRecordQuery());
		if(fsiRec == null)
			return null;
		else
			return FSI_BYPASSABLE_FIELD_IDS_COLUMN.retrieveValue(fsiRec);
	}
	
	private Project getProject(Record projRec)
	{
		if(projRec == null)
			return null;
		
		Project project = null;
		
		// Check cache first:
		project = cache.get(getCacheKey(PROJECT_ID_COLUMN.retrieveValue(projRec).intValue(), PROJECT_FINGERPRINT_COLUMN.retrieveValue(projRec).intValue()));
		
		// Parse project if we didn't get it from the cache: 
		if(project == null)
		{			
			String variant = PROJECT_VARIANT_COLUMN.retrieveValue(projRec);
			project = ProjectLoader.ParseProject(	fileStorageProvider.getProjectInstallationFolder(	PROJECT_NAME_COLUMN.retrieveValue(projRec),
																										variant.isEmpty() ? null : variant, 
																										PROJECT_VERSION_COLUMN.retrieveValue(projRec),
																										false),
													this); // pass this as FormSchemaInfoProvider
			// Check if we have a project:
			if(project == null)
			{
				try
				{
					recordStore.delete(projRec);
				}
				catch(DBException e)
				{
					e.printStackTrace();
				}
			}
			else
				// Add to cache:
				cacheProject(project);
		}
		return project;
	}
	
	private void cacheProject(Project project)
	{
		cache.put(getCacheKey(project.getID(), project.getFingerPrint()), project);
	}
	
	private long getCacheKey(int projectID, int projectFingerPrint)
	{
		return (long) projectID << 32 | projectFingerPrint & 0xFFFFFFFFl;
	}
	
	private List<Project> getProjects(List<Record> projRecs)
	{
		if(projRecs.isEmpty())
			return Collections.<Project> emptyList();
		List<Project> projects = new ArrayList<Project>();
		for(Record projRec : projRecs)
			CollectionUtils.addIgnoreNull(projects, getProject(projRec));
		return projects;
	}
	
	/**
	 * Overridden to avoid that isStored() also does an necessary signature-based query.
	 * The reason is that when the underlying RecordStore enforces indexes signature clashes will automatically be avoided upon insertion.
	 * 
	 * @see #doAdd(Project)
	 * @see RecordStore#hasFullIndexSupport()
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#add(uk.ac.ucl.excites.sapelli.collector.model.Project)
	 */
	@Override
	public Project add(Project project) throws ProjectSignatureClashException, ProjectIdentificationClashException
	{
		if(!isStored(project, !recordStore.hasFullIndexSupport()))
			// Go ahead with storing project:
			doAdd(project);
		// Return if successful:
		return project;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#doAdd(uk.ac.ucl.excites.sapelli.collector.model.Project)
	 */
	@Override
	protected void doAdd(Project project) throws ProjectIdentificationClashException, ProjectSignatureClashException, IllegalStateException
	{
		try
		{	// Note: we use insert() instead of store() to not allow updates
			// Store project record:
			recordStore.insert(getProjectRecord(project));
			// Store an FSI record for each record-producing form:
			for(Form form : project.getForms())
				if(form.isProducesRecords())
					recordStore.insert(getFSIRecord(form));
			// Cache the project:
			cacheProject(project);
		}
		catch(DBPrimaryKeyException dbPKE)
		{
			throw new ProjectIdentificationClashException(project, false);
		}
		catch(DBConstraintException dbCE)
		{
			throw new ProjectSignatureClashException(project);
		}
		catch(DBException e)
		{
			e.printStackTrace(System.err);
			throw new IllegalStateException(e);
		}
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#retrieveProjects()
	 */
	@Override
	public List<Project> retrieveProjects()
	{
		return getProjects(recordStore.retrieveRecords(PROJECT_SCHEMA));
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#retrieveProject(java.lang.String, java.lang.String, java.lang.String)
	 */
	@Override
	public Project retrieveProject(String name, String variant, String version)
	{
		return getProject(recordStore.retrieveRecord(new FirstRecordQuery(	PROJECT_SCHEMA,
																			new EqualityConstraint(PROJECT_NAME_COLUMN, name),
																			new EqualityConstraint(PROJECT_VARIANT_COLUMN, variant != null ? variant : ""),
																			new EqualityConstraint(PROJECT_VERSION_COLUMN, version))));
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#retrieveProject(int, int)
	 */
	@Override
	public Project retrieveProject(int projectID, int projectFingerPrint)
	{
		return getProject(recordStore.retrieveRecord(PROJECT_SCHEMA.createRecordReference(projectID, projectFingerPrint).getRecordQuery()));
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#retrieveV1Project(int, int)
	 */
	@Override
	public Project retrieveV1Project(int schemaID, int schemaVersion)
	{
		return getProject(recordStore.retrieveRecord(new FirstRecordQuery(	PROJECT_SCHEMA,
																			new EqualityConstraint(PROJECT_ID_COLUMN, schemaID),
																			new EqualityConstraint(PROJECT_V1X_SCHEMA_VERSION_COLUMN, schemaVersion))));
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#retrieveProjectVersions(int)
	 */
	@Override
	public List<Project> retrieveProjectVersions(int projectID)
	{
		return getProjects(recordStore.retrieveRecords(new RecordsQuery(Source.From(PROJECT_SCHEMA),
																		Order.By(PROJECT_VERSION_COLUMN),
																		new EqualityConstraint(PROJECT_ID_COLUMN, projectID))));		
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#delete(uk.ac.ucl.excites.sapelli.collector.model.Project)
	 */
	@Override
	public void delete(Project project)
	{
		try
		{
			Constraint projectMatchConstraint = getProjectRecordReference(project).getRecordQueryConstraint();
			// Delete project record:
			recordStore.delete(new RecordsQuery(Source.From(PROJECT_SCHEMA), projectMatchConstraint));
			// Delete associated FSI & HFK records:
			recordStore.delete(new RecordsQuery(Source.From(FSI_SCHEMA), projectMatchConstraint));
			recordStore.delete(new RecordsQuery(Source.From(HFK_SCHEMA), projectMatchConstraint));
			// Remove project from cache:
			cache.remove(getCacheKey(project.getID(), project.getFingerPrint()));
		}
		catch(DBException e)
		{
			e.printStackTrace(System.err);
		}
	}
	
	private Record getHFKRecord(Relationship relationship, RecordReference foreignKey)
	{
		return HFK_SCHEMA.createRecord(	getProjectRecordReference(relationship.form.project),
										relationship.form.getPosition(),
										relationship.form.getFieldPosition(relationship),
										foreignKey.serialise());
	}
	
	private RecordReference getHFKRecordReference(Relationship relationship)
	{
		return HFK_SCHEMA.createRecordReference(getProjectRecordReference(relationship.form.project),
												relationship.form.getPosition(),
												relationship.form.getFieldPosition(relationship)); 
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#storeHeldForeignKey(uk.ac.ucl.excites.sapelli.collector.model.fields.Relationship, uk.ac.ucl.excites.sapelli.storage.model.RecordReference)
	 */
	@Override
	public void storeHeldForeignKey(Relationship relationship, RecordReference foreignKey)
	{
		try
		{
			recordStore.store(getHFKRecord(relationship, foreignKey));
		}
		catch(DBException e)
		{
			e.printStackTrace();
		}		
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#retrieveHeldForeignKey(uk.ac.ucl.excites.sapelli.collector.model.fields.Relationship)
	 */
	@Override
	public RecordReference retrieveHeldForeignKey(Relationship relationship)
	{
		Record hfkRecord = null;
		try
		{
			hfkRecord = recordStore.retrieveRecord(getHFKRecordReference(relationship).getRecordQuery());
			return relationship.getRelatedForm().getSchema().createRecordReference(HFK_SERIALISED_RECORD_REFERENCE.retrieveValue(hfkRecord));
		}
		catch(Exception e)
		{
			if(hfkRecord != null)
				deleteHeldForeignKey(relationship);
		}
		//else:
		return null;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#deleteHeldForeignKey(uk.ac.ucl.excites.sapelli.collector.model.fields.Relationship)
	 */
	@Override
	public void deleteHeldForeignKey(Relationship relationship)
	{
		try
		{
			recordStore.delete(getHFKRecordReference(relationship));
		}
		catch(DBException e)
		{
			e.printStackTrace();
		}
	}
	
	private Record getSendScheduleRecord(SendingSchedule schedule, TransmissionStore transmissionStore)
	{
		Record record = SEND_RECORDS_SCHEDULE_SCHEMA.createRecord();
		SEND_RECORDS_SCHEDULE_COLUMN_PROJECT.storeValue(record, getProjectRecordReference(schedule.getProject()));
		SEND_RECORDS_SCHEDULE_COLUMN_RECEIVER.storeValue(record, transmissionStore.getCorrespondentRecord(schedule.getReceiver()).getReference());
		SEND_RECORDS_SCHEDULE_COLUMN_INTERVAL.storeValue(record, schedule.getTransmitIntervalS());
		SEND_RECORDS_SCHEDULE_COLUMN_ENCRYPT.storeValue(record, schedule.isEncrypt());
		return record;
	}
	
	@Override
	public void storeSendSchedule(SendingSchedule schedule, TransmissionStore transmissionStore)
	{
		try
		{
			recordStore.store(getSendScheduleRecord(schedule, transmissionStore));
		}
		catch(DBException e)
		{
			e.printStackTrace();
		}
	}

	@Override
	public SendingSchedule retrieveSendScheduleForProject(Project project, TransmissionStore transmissionStore)
	{
		return getSendScheduleFromRecord(project, transmissionStore, recordStore.retrieveRecord(new FirstRecordQuery(SEND_RECORDS_SCHEDULE_SCHEMA, 
				new EqualityConstraint(SEND_RECORDS_SCHEDULE_COLUMN_PROJECT, getProjectRecordReference(project)))));
	}
	
	@Override
	public void deleteSendSchedule(SendingSchedule schedule)
	{
		try
		{
			recordStore.delete(new FirstRecordQuery(SEND_RECORDS_SCHEDULE_SCHEMA, new EqualityConstraint(SEND_RECORDS_SCHEDULE_COLUMN_PROJECT, schedule.getProject().getID())));
		}
		catch(DBException e)
		{
			e.printStackTrace();
		}
	}
	
	private SendingSchedule getSendScheduleFromRecord(Project project, TransmissionStore transmissionStore, Record sendScheduleRecord)
	{
		if (sendScheduleRecord == null)
			return null;
		
		Correspondent receiver = transmissionStore.retrieveCorrespondentByQuery(new FirstRecordQuery(TransmissionStore.RECEIVER_SCHEMA,
				SEND_RECORDS_SCHEDULE_COLUMN_RECEIVER.retrieveValue(sendScheduleRecord).getRecordQueryConstraint())); // TODO check
		
		if (receiver == null)
			return null;
		
		Integer retransmitIntervalMillis = SEND_RECORDS_SCHEDULE_COLUMN_INTERVAL.retrieveValue(sendScheduleRecord).intValue();
		Boolean encrypt = SEND_RECORDS_SCHEDULE_COLUMN_ENCRYPT.retrieveValue(sendScheduleRecord);

		return new SendingSchedule(project, receiver, retransmitIntervalMillis, encrypt);
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
