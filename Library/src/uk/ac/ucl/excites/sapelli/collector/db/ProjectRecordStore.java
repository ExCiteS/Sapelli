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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.IOUtils;

import uk.ac.ucl.excites.sapelli.collector.CollectorClient;
import uk.ac.ucl.excites.sapelli.collector.db.exceptions.ProjectIdentificationClashException;
import uk.ac.ucl.excites.sapelli.collector.db.exceptions.ProjectSignatureClashException;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.load.FormSchemaInfoProvider;
import uk.ac.ucl.excites.sapelli.collector.load.ProjectLoader;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.model.ProjectDescriptor;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Relationship;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendSchedule;
import uk.ac.ucl.excites.sapelli.shared.db.StoreBackupper;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBConstraintException;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBPrimaryKeyException;
import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitWrapInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitWrapOutputStream;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStoreWrapper;
import uk.ac.ucl.excites.sapelli.storage.model.ColumnSet;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.RecordReference;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSet;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ByteArrayColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ByteArrayListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.indexes.AutoIncrementingPrimaryKey;
import uk.ac.ucl.excites.sapelli.storage.model.indexes.Index;
import uk.ac.ucl.excites.sapelli.storage.model.indexes.PrimaryKey;
import uk.ac.ucl.excites.sapelli.storage.queries.FirstRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Order;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.DummyConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.EqualityConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.sources.Source;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;
import uk.ac.ucl.excites.sapelli.transmission.model.Transmission;

/**
 * A RecordStore based implementation of ProjectStore
 * 
 * @author mstevens
 */
public class ProjectRecordStore extends ProjectStore implements FormSchemaInfoProvider, StoreHandle.StoreUser
{
	
	// STATICS---------------------------------------------
	// Project storage model:
	//	Model:
	static public final Model COLLECTOR_MANAGEMENT_MODEL = new Model(CollectorClient.COLLECTOR_MANAGEMENT_MODEL_ID, "CollectorManagement", CollectorClient.SCHEMA_FLAGS_COLLECTOR_INTERNAL);
	//	 Project schema:
	static public final Schema PROJECT_SCHEMA = CollectorClient.CreateSchemaWithSuffixedTableName(COLLECTOR_MANAGEMENT_MODEL, Project.class.getSimpleName(), "s");
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
		PROJECT_SCHEMA.setPrimaryKey(PrimaryKey.WithColumnNames(PROJECT_ID_COLUMN, PROJECT_FINGERPRINT_COLUMN), true /*seal!*/);
	}
	//	 Form Schema Info (FSI) schema:
	static public final Schema FSI_SCHEMA = CollectorClient.CreateSchema(COLLECTOR_MANAGEMENT_MODEL, "FormSchemaInfo");
	//		Columns:
	static private final ForeignKeyColumn FSI_PROJECT_KEY_COLUMN = FSI_SCHEMA.addColumn(new ForeignKeyColumn(PROJECT_SCHEMA, false));
	static private final IntegerColumn FSI_FORM_POSITION_COLUMN = FSI_SCHEMA.addColumn(new IntegerColumn("formPosition", false, 0, Project.MAX_FORMS - 1));
	static public final StringListColumn FSI_BYPASSABLE_FIELD_IDS_COLUMN = FSI_SCHEMA.addColumn(new StringListColumn("byPassableFieldIDs", StringColumn.ForCharacterCount("FieldID", false, Field.MAX_ID_LENGTH), true, Form.MAX_FIELDS));
	//		Set primary key & seal schema:
	static
	{
		FSI_SCHEMA.setPrimaryKey(PrimaryKey.WithColumnNames(FSI_PROJECT_KEY_COLUMN, FSI_FORM_POSITION_COLUMN), true /*seal!*/);
	}
	//	 Held Foreign Key (HFK) schema: to store "held" foreign keys (RecordReferences) on Relationship fields
	static public final Schema HFK_SCHEMA = CollectorClient.CreateSchemaWithSuffixedTableName(COLLECTOR_MANAGEMENT_MODEL, "RelationshipFK", "s");
	//		Columns:
	static private final ForeignKeyColumn HFK_PROJECT_KEY_COLUMN = HFK_SCHEMA.addColumn(new ForeignKeyColumn(PROJECT_SCHEMA, false));
	static private final IntegerColumn HFK_FORM_POSITION_COLUMN = HFK_SCHEMA.addColumn(new IntegerColumn("formPosition", false, 0, Project.MAX_FORMS - 1));
	static private final IntegerColumn HFK_RELATIONSHIP_FIELD_POSITION_COLUMN = HFK_SCHEMA.addColumn(new IntegerColumn("relationshipFieldPosition", false, 0, Form.MAX_FIELDS - 1));
	static private final StringColumn HFK_SERIALISED_RECORD_REFERENCE = HFK_SCHEMA.addColumn(StringColumn.ForCharacterCount("serialisedRecordReference", false, 256));
	//		Set primary key & seal schema:
	static
	{
		HFK_SCHEMA.setPrimaryKey(PrimaryKey.WithColumnNames(HFK_PROJECT_KEY_COLUMN, HFK_FORM_POSITION_COLUMN, HFK_RELATIONSHIP_FIELD_POSITION_COLUMN), true /*seal!*/);
	}
	
	// ColumnSet & columns used for Project serialisation (see serialise() & deserialise()): 
	static private final ColumnSet PROJECT_SERIALISIATION_CS = new ColumnSet("ProjectSerialisation", false);
	static private final ByteArrayColumn PROJECT_SERIALISIATION_XML_COLUMN = PROJECT_SERIALISIATION_CS.addColumn(new ByteArrayColumn("ProjectXMLBytes", false));
	static private final ByteArrayListColumn PROJECT_SERIALISIATION_FSI_RECORDS_COLUMN = PROJECT_SERIALISIATION_CS.addColumn(new ByteArrayListColumn("ProjectFSIRecordsBytes", false, Project.MAX_FORMS));
	static
	{
		PROJECT_SERIALISIATION_CS.seal();
	}
	
	//	Record-sending schedule Schema
	static final public Schema SEND_SCHEDULE_SCHEMA = new Schema(COLLECTOR_MANAGEMENT_MODEL, SendSchedule.class.getSimpleName(), SendSchedule.class.getSimpleName() + "s");
	//		Columns:
	static final public IntegerColumn SEND_SCHEDULE_COLUMN_ID = SEND_SCHEDULE_SCHEMA.addColumn(new IntegerColumn("ID", false));
	static final public ForeignKeyColumn SEND_SCHEDULE_COLUMN_PROJECT = SEND_SCHEDULE_SCHEMA.addColumn(new ForeignKeyColumn("Project", ProjectRecordStore.PROJECT_SCHEMA, false));
	static final public ForeignKeyColumn SEND_SCHEDULE_COLUMN_RECEIVER = SEND_SCHEDULE_SCHEMA.addColumn(new ForeignKeyColumn("Receiver", TransmissionStore.CORRESPONDENT_SCHEMA, false));
	static final public IntegerColumn SEND_SCHEDULE_COLUMN_INTERVAL = SEND_SCHEDULE_SCHEMA.addColumn(new IntegerColumn("RetransmitInterval", false, false)); // unsigned 32 bits
	static final public BooleanColumn SEND_SCHEDULE_COLUMN_ENCRYPT = SEND_SCHEDULE_SCHEMA.addColumn(new BooleanColumn("Encrypt", false));
	static final public BooleanColumn SEND_SCHEDULE_COLUMN_ENABLED = SEND_SCHEDULE_SCHEMA.addColumn(new BooleanColumn("Enabled", false));
	static final public IntegerColumn SEND_SCHEDULE_COLUMN_HEARTBEAT_INTERVAL = SEND_SCHEDULE_SCHEMA.addColumn(new IntegerColumn("HeartbeatInterval", false, false, 0l)); // unsigned 32 bits
	static final public IntegerColumn SEND_SCHEDULE_COLUMN_NO_DATA_COUNTER = SEND_SCHEDULE_SCHEMA.addColumn(new IntegerColumn("NoDataCounter", false, false, 0l)); // unsigned 32 bits
	//		Set primary key & seal schema:
	static
	{
		// Add index to enforce unique combination of project and correspondent:
		SEND_SCHEDULE_SCHEMA.addIndex(new Index("IdxProjectCorrespondent", true, SEND_SCHEDULE_COLUMN_PROJECT, SEND_SCHEDULE_COLUMN_RECEIVER));
		SEND_SCHEDULE_SCHEMA.setPrimaryKey(new AutoIncrementingPrimaryKey("IDIdx", SEND_SCHEDULE_COLUMN_ID), true /*seal!*/);
	}
	
	//	Record-receiving Schema
	static final public Schema RECEIVING_PROJECT_SCHEMA = new Schema(COLLECTOR_MANAGEMENT_MODEL, "ReceivingProject", "ReceivingProjects");
	//		Columns:
	static final public ForeignKeyColumn RECEIVING_PROJECT_COLUMN_PROJECT = RECEIVING_PROJECT_SCHEMA.addColumn(new ForeignKeyColumn("Project", ProjectRecordStore.PROJECT_SCHEMA, false));
	static final public IntegerColumn RECEIVING_PROJECT_COLUMN_TRANSMISSION_TYPE = RECEIVING_PROJECT_SCHEMA.addColumn(new IntegerColumn("TransmissionType", false));
	static final public BooleanColumn RECEIVING_PROJECT_COLUMN_ENABLED = RECEIVING_PROJECT_SCHEMA.addColumn(new BooleanColumn("Enabled", false, false));
	//		Set primary key & seal schema:
	static
	{
		// Add index to enforce unique combination of project and correspondent:
		RECEIVING_PROJECT_SCHEMA.setPrimaryKey(PrimaryKey.WithColumnNames(RECEIVING_PROJECT_COLUMN_PROJECT, RECEIVING_PROJECT_COLUMN_TRANSMISSION_TYPE), true /*seal!*/);
	}
	
	// Seal the model itself:
	static
	{
		COLLECTOR_MANAGEMENT_MODEL.seal();
	}
			
	// DYNAMICS--------------------------------------------
	private final CollectorClient client;
	private final RecordStoreWrapper<CollectorClient> rsWrapper;
	private final TransmissionStore transmissionStore;
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
		this.rsWrapper = new RecordStoreWrapper<CollectorClient>(client);
		this.transmissionStore = client.transmissionStoreHandle.getStore(this);
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
	
	private RecordReference getProjectRecordReference(ProjectDescriptor projDescr)
	{
		return PROJECT_SCHEMA.createRecordReference(projDescr.getID(),
													projDescr.getFingerPrint());
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
	
	private Record retrieveFSIRecord(Form form)
	{
		return rsWrapper.recordStore.retrieveRecord(getFSIRecordReference(form));
	}
	
	public List<String> getByPassableFieldIDs(Record fsiRec)
	{
		if(fsiRec == null)
			return null;
		else
			return FSI_BYPASSABLE_FIELD_IDS_COLUMN.retrieveValue(fsiRec);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.load.FormSchemaInfoProvider#getByPassableFieldIDs(uk.ac.ucl.excites.sapelli.collector.model.Form)
	 */
	@Override
	public List<String> getByPassableFieldIDs(Form form)
	{
		return getByPassableFieldIDs(retrieveFSIRecord(form));
	}
	
	private ProjectDescriptor getProjectOrDescriptor(Record projRec)
	{
		if(projRec == null)
			return null;
		
		// Create ProjectDescriptor:
		int id = PROJECT_ID_COLUMN.retrieveValue(projRec).intValue();
		boolean v1x = PROJECT_V1X_SCHEMA_VERSION_COLUMN.isValuePresent(projRec);
		ProjectDescriptor projDescr = new ProjectDescriptor(v1x ? ProjectDescriptor.PROJECT_ID_V1X_TEMP : id,
															PROJECT_NAME_COLUMN.retrieveValue(projRec),
															PROJECT_VARIANT_COLUMN.retrieveValue(projRec), // "" is treated as null,
															PROJECT_VERSION_COLUMN.retrieveValue(projRec),
															PROJECT_FINGERPRINT_COLUMN.retrieveValue(projRec).intValue());
		if(v1x)
			projDescr.setV1XSchemaInfo(id, PROJECT_V1X_SCHEMA_VERSION_COLUMN.retrieveValue(projRec).intValue());
		
		// If the full project is cached return it instead of the descriptor:
		Project project = cache.get(getCacheKey(projDescr));
		if(project != null)
			return project;
		
		return projDescr;
	}
	
	private Project getProject(Record projRec)
	{
		return loadProject(getProjectOrDescriptor(projRec));
	}
	
	private File getProjectFolder(ProjectDescriptor projDescr)
	{
		return fileStorageProvider.getProjectInstallationFolder(projDescr.getName(),
																projDescr.getVariant(), 
																projDescr.getVersion(),
																false);
	}
	
	private Project loadProject(ProjectDescriptor projDescr)
	{
		if(projDescr == null)
			return null;
		
		// If the ProjectDescriptor is a full Project:
		if(projDescr instanceof Project)
			return (Project) projDescr;
		
		// Load project...
		Project project = null;
		
		// First check the cache:
		project = cache.get(getCacheKey(projDescr));
		
		// Parse project if we didn't get it from the cache: 
		if(project == null)
		{
			project = ProjectLoader.ParseProjectXMLInFolder(getProjectFolder(projDescr), this); // pass this as FormSchemaInfoProvider
			// Check if we have a project:
			if(project == null)
				delete(projDescr);
			else
				// Add to cache:
				cacheProject((Project) project);
		}
		return project;
	}
	
	private void cacheProject(Project project)
	{
		cache.put(getCacheKey(project), project);
	}
	
	private long getCacheKey(ProjectDescriptor projDescr)
	{
		return CollectorClient.GetModelID(projDescr);
	}
	
	private List<Project> getProjects(List<Record> projRecs)
	{
		if(projRecs.isEmpty())
			return Collections.<Project> emptyList();
		List<Project> projects = new ArrayList<Project>(projRecs.size());
		for(Record projRec : projRecs)
			CollectionUtils.addIgnoreNull(projects, loadProject(getProjectOrDescriptor(projRec)));
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
		if(!isStored(project, !rsWrapper.recordStore.hasFullIndexSupport()))
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
			rsWrapper.recordStore.insert(getProjectRecord(project));
			// Store an FSI record for each record-producing form:
			for(Form form : project.getForms())
				if(form.isProducesRecords())
					rsWrapper.recordStore.insert(getFSIRecord(form));
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
		return getProjects(rsWrapper.recordStore.retrieveRecords(PROJECT_SCHEMA));
	}
	
	@Override
	public List<ProjectDescriptor> retrieveProjectsOrDescriptors()
	{
		List<Record> projRecs = rsWrapper.recordStore.retrieveRecords(PROJECT_SCHEMA);
		if(projRecs.isEmpty())
			return Collections.<ProjectDescriptor> emptyList();
		List<ProjectDescriptor> projDescrs = new ArrayList<ProjectDescriptor>(projRecs.size());
		for(Record projRec : projRecs)
			CollectionUtils.addIgnoreNull(projDescrs, getProjectOrDescriptor(projRec));
		return projDescrs;
	}

	@Override
	public Project retrieveProject(ProjectDescriptor descriptor)
	{
		return loadProject(descriptor);
	}
	
	protected Project retrieveProject(RecordReference projectRecordReference)
	{
		return getProject(rsWrapper.recordStore.retrieveRecord(projectRecordReference));
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#retrieveProject(java.lang.String, java.lang.String, java.lang.String)
	 */
	@Override
	public Project retrieveProject(String name, String variant, String version)
	{
		return getProject(rsWrapper.recordStore.retrieveRecord(new FirstRecordQuery(PROJECT_SCHEMA,
																					new EqualityConstraint(PROJECT_NAME_COLUMN, name),
																					new EqualityConstraint(PROJECT_VARIANT_COLUMN, variant != null ? variant : ""),
																					new EqualityConstraint(PROJECT_VERSION_COLUMN, version))));
	}

	private Record queryProjectRecordByIDFingerPrint(int projectID, int projectFingerPrint)
	{
		return rsWrapper.recordStore.retrieveRecord(PROJECT_SCHEMA.createRecordReference(projectID, projectFingerPrint).getRecordQuery());
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#retrieveProject(int, int)
	 */
	@Override
	public Project retrieveProject(int projectID, int projectFingerPrint)
	{
		return getProject(queryProjectRecordByIDFingerPrint(projectID, projectFingerPrint));
	}
	
	@Override
	public ProjectDescriptor retrieveProjectOrDescriptor(int projectID, int projectFingerPrint)
	{
		return getProjectOrDescriptor(queryProjectRecordByIDFingerPrint(projectID, projectFingerPrint));
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#retrieveV1Project(int, int)
	 */
	@Override
	public Project retrieveV1Project(int schemaID, int schemaVersion)
	{
		return getProject(rsWrapper.recordStore.retrieveRecord(new FirstRecordQuery(PROJECT_SCHEMA,
																					new EqualityConstraint(PROJECT_ID_COLUMN, schemaID),
																					new EqualityConstraint(PROJECT_V1X_SCHEMA_VERSION_COLUMN, schemaVersion))));
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#retrieveProjectVersions(int)
	 */
	@Override
	public List<Project> retrieveProjectVersions(int projectID)
	{
		return getProjects(rsWrapper.recordStore.retrieveRecords(new RecordsQuery(Source.From(PROJECT_SCHEMA),
																		Order.By(PROJECT_VERSION_COLUMN),
																		new EqualityConstraint(PROJECT_ID_COLUMN, projectID))));		
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#delete(uk.ac.ucl.excites.sapelli.collector.model.Project)
	 */
	@Override
	public void delete(Project project)
	{
		delete((ProjectDescriptor) project);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#delete(uk.ac.ucl.excites.sapelli.collector.model.Project)
	 */
	@Override
	public void delete(ProjectDescriptor projectDescriptor)
	{
		try
		{
			Constraint projectMatchConstraint = getProjectRecordReference(projectDescriptor).getRecordQueryConstraint();
			// Delete project record:
			rsWrapper.recordStore.delete(new RecordsQuery(Source.From(PROJECT_SCHEMA), projectMatchConstraint));
			// Delete associated FSI, HFK, SendSchedule & Receiving records:
			rsWrapper.recordStore.delete(new RecordsQuery(FSI_SCHEMA, projectMatchConstraint));
			rsWrapper.recordStore.delete(new RecordsQuery(HFK_SCHEMA, projectMatchConstraint));
			rsWrapper.recordStore.delete(new RecordsQuery(SEND_SCHEDULE_SCHEMA, projectMatchConstraint));
			rsWrapper.recordStore.delete(new RecordsQuery(RECEIVING_PROJECT_SCHEMA, projectMatchConstraint));
			// Remove project from cache:
			cache.remove(getCacheKey(projectDescriptor));
		}
		catch(DBException e)
		{
			rsWrapper.client.logError("Error upon deleting project: " + projectDescriptor.toString(false), e);
		}
	}
	

	@Override
	public void deleteSendSchedulesForProject(Project project)
	{
		try
		{
			if(project != null)
				rsWrapper.recordStore.delete(new RecordsQuery(SEND_SCHEDULE_SCHEMA, new EqualityConstraint(SEND_SCHEDULE_COLUMN_PROJECT, getProjectRecordReference(project))));
		}
		catch(DBException e)
		{
			rsWrapper.client.logError("Error upon deleting SendSchedules for Project \"" + project.toString(false) + "\".", e);
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
			rsWrapper.recordStore.store(getHFKRecord(relationship, foreignKey));
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
			hfkRecord = rsWrapper.recordStore.retrieveRecord(getHFKRecordReference(relationship));
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
			rsWrapper.recordStore.delete(getHFKRecordReference(relationship));
		}
		catch(DBException e)
		{
			e.printStackTrace();
		}
	}

	@Override
	public void storeSendSchedule(SendSchedule schedule)
	{
		try
		{
			// Get record representation:
			Record rec = SEND_SCHEDULE_SCHEMA.createRecord();
			if(schedule.isIDSet())
				SEND_SCHEDULE_COLUMN_ID.storeValue(rec, schedule.getID());
			SEND_SCHEDULE_COLUMN_PROJECT.storeValue(rec, getProjectRecordReference(schedule.getProject()));
			SEND_SCHEDULE_COLUMN_RECEIVER.storeValue(rec, transmissionStore.getCorrespondentRecordReference(schedule.getReceiver(), true, false));
			SEND_SCHEDULE_COLUMN_INTERVAL.storeValue(rec, schedule.getTransmitIntervalS());
			SEND_SCHEDULE_COLUMN_ENCRYPT.storeValue(rec, schedule.isEncrypt());
			SEND_SCHEDULE_COLUMN_ENABLED.storeValue(rec, schedule.isEnabled());
			SEND_SCHEDULE_COLUMN_HEARTBEAT_INTERVAL.storeValue(rec, schedule.getHeartbeatInterval());
			SEND_SCHEDULE_COLUMN_NO_DATA_COUNTER.storeValue(rec, schedule.getNoDataToSendCounter());
			
			// Store/update the record in the db:
			rsWrapper.recordStore.store(rec);
			
			// Check/set id on the object:
			if(schedule.isIDSet()) // if the object already had an ID...
			{	// then it should match the ID on the record, so let's verify:
				if(schedule.getID() != SEND_SCHEDULE_COLUMN_ID.retrieveValue(rec).intValue())
					throw new IllegalStateException("Non-matching schedule ID"); // this should never happen
			}
			else
				// Set id in object as on the record: 
				schedule.setID(SEND_SCHEDULE_COLUMN_ID.retrieveValue(rec).intValue());
		}
		catch(DBException e)
		{
			client.logError("Error upon storing send schedule", e);
		}
	}

	@Override
	public SendSchedule retrieveSendScheduleByID(int id)
	{
		try
		{
			Record sendScheduleRecord = rsWrapper.recordStore.retrieveRecord(SEND_SCHEDULE_SCHEMA.createRecordReference(id));
			return getSendScheduleFromRecord(transmissionStore, sendScheduleRecord);
		}
		catch(Exception e)
		{
			rsWrapper.client.logError("Error upon retrieving sendSchedule", e);
			return null;
		}
	}
	
	@Override
	public List<SendSchedule> retrieveSendSchedulesForProject(ProjectDescriptor projDescr)
	{
		return retrieveSendSchedules(getProjectRecordReference(projDescr).getRecordQueryConstraint());
	}
	
	private Constraint getReceiverConstraint(Correspondent receiver)
	{
		Constraint constr = null;
		try
		{
			constr = transmissionStore.getCorrespondentRecordReference(receiver, false, false).getRecordQueryConstraint();
		}
		catch(DBException ignore) {}
		// Check & return:
		return constr != null ? constr : DummyConstraint.ACCEPT_NONE;
	}
	
	@Override
	public List<SendSchedule> retrieveSendSchedulesForReceiver(Correspondent receiver)
	{
		return retrieveSendSchedules(getReceiverConstraint(receiver));
	}
	
	@Override
	public List<SendSchedule> retrieveEnabledSendSchedules(Transmission.Type transmissionType)
	{
		List<SendSchedule> schedules = new ArrayList<SendSchedule>();
		for(SendSchedule schedule : retrieveSendSchedules(new EqualityConstraint(SEND_SCHEDULE_COLUMN_ENABLED, Boolean.TRUE)))
			if(schedule.getProject() != null && schedule.getReceiver() != null && (transmissionType == null || schedule.getReceiver().getTransmissionType() == transmissionType))
				schedules.add(schedule);
		return schedules;
	}
	
	private List<SendSchedule> retrieveSendSchedules(Constraint constraint)
	{
		List<Record> sendScheduleRecords = rsWrapper.recordStore.retrieveRecords(new RecordsQuery(SEND_SCHEDULE_SCHEMA, constraint));
		List<SendSchedule> schedules = new ArrayList<SendSchedule>(sendScheduleRecords.size());
		for(Record sendScheduleRecord : sendScheduleRecords)
			CollectionUtils.addIgnoreNull(schedules, getSendScheduleFromRecord(transmissionStore, sendScheduleRecord));
		return schedules;
	}
	
	@Override
	public void deleteSendSchedule(SendSchedule sendSchedule)
	{
		try
		{
			if(sendSchedule.isIDSet())
				rsWrapper.recordStore.delete(SEND_SCHEDULE_SCHEMA.createRecordReference(sendSchedule.getID()));
		}
		catch(DBException e)
		{
			client.logError("Error upon deleting sendSchedule", e);
		}
	}

	/**
	 * Note: may return SendSchedules with a null of userDeleted receiver.
	 * 
	 * @param transmissionStore
	 * @param sendScheduleRecord
	 * @return
	 */
	private SendSchedule getSendScheduleFromRecord(TransmissionStore transmissionStore, Record sendScheduleRecord)
	{
		if(sendScheduleRecord == null)
			return null;
		Project project = retrieveProject(SEND_SCHEDULE_COLUMN_PROJECT.retrieveValue(sendScheduleRecord));
		Correspondent receiver = transmissionStore.retrieveCorrespondentByQuery(SEND_SCHEDULE_COLUMN_RECEIVER.retrieveValue(sendScheduleRecord).getRecordQuery());
		return new SendSchedule(project,
								SEND_SCHEDULE_COLUMN_ID.retrieveValue(sendScheduleRecord).intValue(),
								receiver /* may be null or userDeleted*/,
								SEND_SCHEDULE_COLUMN_INTERVAL.retrieveValue(sendScheduleRecord).intValue(),
								SEND_SCHEDULE_COLUMN_ENCRYPT.retrieveValue(sendScheduleRecord),
								SEND_SCHEDULE_COLUMN_ENABLED.retrieveValue(sendScheduleRecord),
								SEND_SCHEDULE_COLUMN_HEARTBEAT_INTERVAL.retrieveValue(sendScheduleRecord).intValue(),
								SEND_SCHEDULE_COLUMN_NO_DATA_COUNTER.retrieveValue(sendScheduleRecord).intValue());
	}
	
	@Override
	public boolean isReceiving(Transmission.Type transmissionType)
	{
		return !rsWrapper.recordStore.retrieveRecords(
			new RecordsQuery(
				RECEIVING_PROJECT_SCHEMA,
				new EqualityConstraint(RECEIVING_PROJECT_COLUMN_TRANSMISSION_TYPE, transmissionType /*Enum will be converted to Long*/),
				new EqualityConstraint(RECEIVING_PROJECT_COLUMN_ENABLED, Boolean.TRUE)))
			.isEmpty();
	}
	
	@Override
	public boolean isReceiving(ProjectDescriptor projectDesc, Transmission.Type transmissionType)
	{
		try
		{
			Record receivingRec = rsWrapper.recordStore.retrieveRecord(
				RECEIVING_PROJECT_SCHEMA.createRecordReference(
					getProjectRecordReference(projectDesc),
					transmissionType.ordinal()).getRecordQuery());
			if(receivingRec == null)
				return false;
			else
				return RECEIVING_PROJECT_COLUMN_ENABLED.retrieveValue(receivingRec);
		}
		catch(Exception e)
		{
			client.logError("Error upon retrieving receiving state for project \"" + projectDesc.toString(false) + "\" and transmission type " + transmissionType.name(), e);
			return false;
		}
	}

	@Override
	public void setReceiving(ProjectDescriptor projectDesc, Transmission.Type transmissionType, boolean enabled)
	{
		try
		{
			// Get record representation:
			Record rec = RECEIVING_PROJECT_SCHEMA.createRecord();
			RECEIVING_PROJECT_COLUMN_PROJECT.storeValue(rec, getProjectRecordReference(projectDesc));
			RECEIVING_PROJECT_COLUMN_TRANSMISSION_TYPE.storeValue(rec, transmissionType.ordinal());
			RECEIVING_PROJECT_COLUMN_ENABLED.storeValue(rec, enabled);
			
			// Store/update the record in the db:
			rsWrapper.recordStore.store(rec);
		}
		catch(DBException e)
		{
			client.logError("Error upon setting receiving state for project \"" + projectDesc.toString(false) + "\" and transmission type " + transmissionType.name(), e);
		}
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#getTransmissionStore()
	 */
	@Override
	public TransmissionStore getTransmissionStore()
	{
		return transmissionStore;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.shared.db.Store#finalise()
	 */
	@Override
	protected void doClose() throws DBException
	{
		rsWrapper.doClose();
		client.transmissionStoreHandle.doneUsing(this);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.shared.db.Store#backup(uk.ac.ucl.excites.sapelli.shared.db.StoreBackuper, java.io.File)
	 */
	@Override
	public void backup(StoreBackupper backuper, File destinationFolder) throws DBException
	{
		rsWrapper.backup(backuper, destinationFolder);
		backuper.addStoreForBackup(transmissionStore);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#serialise(uk.ac.ucl.excites.sapelli.collector.model.Project, java.io.OutputStream)
	 */
	@Override
	public void serialise(Project project, OutputStream out) throws IOException
	{
		// Project XML bytes:
		InputStream projectXMLFileIn = new FileInputStream(ProjectLoader.GetProjectXMLFile(getProjectFolder(project)));
		ByteArrayOutputStream projectXMLBytesOut = new ByteArrayOutputStream();
		IOUtils.copy(projectXMLFileIn, projectXMLBytesOut);
		projectXMLFileIn.close();
		projectXMLBytesOut.close();
		
		// FSI record bytes for each Form:
		List<byte[]> fsiRecordBytesList = new ArrayList<>(project.getNumberOfForms());
		for(Form form : project.getForms())
		{
			Record fsiRecord = retrieveFSIRecord(form); // we query the projectStore to save time...
			if(fsiRecord == null) // ... but we don't rely on it:
				fsiRecord = getFSIRecord(form); // may trigger optionality analysis
			fsiRecordBytesList.add(fsiRecord.toBytes(true));
		}
		
		// Create serialisedProject valueSet:
		ValueSet<ColumnSet> serialisedProjectVS = new ValueSet<ColumnSet>(PROJECT_SERIALISIATION_CS, projectXMLBytesOut.toByteArray(), fsiRecordBytesList);
		
		// Return as byte array:
		BitOutputStream bitsOut = new BitWrapOutputStream(out);
		serialisedProjectVS.writeToBitStream(bitsOut, true);
		bitsOut.flush();
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#deserialise(java.io.InputStream)
	 */
	@Override
	public Project deserialise(InputStream in) throws IOException
	{
		// Deserialise valueSet:
		BitInputStream bitsIn = new BitWrapInputStream(in);
		ValueSet<ColumnSet> serialisedProjectVS = new ValueSet<ColumnSet>(PROJECT_SERIALISIATION_CS);
		serialisedProjectVS.readFromBitStream(bitsIn, true);
		bitsIn.close();
		
		// Retrieve FSI records:
		List<byte[]> fsiRecordBytesList = PROJECT_SERIALISIATION_FSI_RECORDS_COLUMN.retrieveValue(serialisedProjectVS);
		final List<Record> fsiRecords = new ArrayList<Record>(fsiRecordBytesList.size());
		for(byte[] fsiRecordBytes : fsiRecordBytesList)
			fsiRecords.add(FSI_SCHEMA.createRecord(fsiRecordBytes, true));
	
		// Retrieve & parse Project XML:
		return ProjectLoader.ParseProjectXML(
			new ByteArrayInputStream(PROJECT_SERIALISIATION_XML_COLUMN.retrieveValue(serialisedProjectVS)),
			new FormSchemaInfoProvider()
			{
				@Override
				public List<String> getByPassableFieldIDs(Form form)
				{
					for(Record fsiRecord : fsiRecords)
						if(FSI_FORM_POSITION_COLUMN.retrieveValue(fsiRecord).shortValue() == form.getPosition())
							return ProjectRecordStore.this.getByPassableFieldIDs(fsiRecord);
					return null;
				}
			});
	}
	
}
