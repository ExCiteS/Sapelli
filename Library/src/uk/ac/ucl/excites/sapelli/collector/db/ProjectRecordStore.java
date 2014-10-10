/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.db;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import uk.ac.ucl.excites.sapelli.collector.SapelliCollectorClient;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Relationship;
import uk.ac.ucl.excites.sapelli.collector.util.DuplicateException;
import uk.ac.ucl.excites.sapelli.shared.db.DBConstraintException;
import uk.ac.ucl.excites.sapelli.shared.db.DBException;
import uk.ac.ucl.excites.sapelli.shared.db.StoreClient;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.Index;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.PrimaryKey;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.RecordReference;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.storage.queries.FirstRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Order;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Source;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.EqualityConstraint;

/**
 * A RecordStore based implementation of ProjectStore
 * 
 * @author mstevens
 */
public class ProjectRecordStore extends ProjectStore implements StoreClient
{
	// STATICS---------------------------------------------
	// Project storage model:
	//	Model:
	static public final Model COLLECTOR_MANAGEMENT_MODEL = new Model(SapelliCollectorClient.COLLECTOR_MANAGEMENT_MODEL_ID, "CollectorManagement");
	//	 Project schema:
	static public final Schema PROJECT_SCHEMA = new Schema(COLLECTOR_MANAGEMENT_MODEL, "Project");
	//		Columns:
	static private final IntegerColumn PROJECT_ID_COLUMN = new IntegerColumn("id", false, Project.PROJECT_ID_FIELD);
	static private final IntegerColumn PROJECT_FINGERPRINT_COLUMN = new IntegerColumn("fingerPrint", false, true, Project.PROJECT_FINGERPRINT_SIZE);
	static private final StringColumn PROJECT_NAME_COLUMN = StringColumn.ForCharacterCount("name", false, 128);
	/*			Note on variant column:
	 * 				Even though project#variant can be null we make the column non-optional (and will store nulls as ""),
	 * 				the reason is that this allows us to enforce a unique index on (name, variant, version), to avoid
	 * 				projects with duplicate signature. Such an index would not behave in this desired way when one of the
	 * 				columns in nullable, as per the SQL standard and common implementations thereof (see http://www.sqlite.org/faq.html#q26) */
	static private final StringColumn PROJECT_VARIANT_COLUMN = StringColumn.ForCharacterCount("variant", false /*see comment*/, 128);
	static private final StringColumn PROJECT_VERSION_COLUMN = StringColumn.ForCharacterCount("version", false, 32);
	static private final IntegerColumn PROJECT_V1X_SCHEMA_VERSION_COLUMN = new IntegerColumn("v1xSchemaVersion", true, Schema.V1X_SCHEMA_VERSION_FIELD);
	//		Primary key:
	static private final PrimaryKey PROJECT_KEY = PrimaryKey.WithColumnNames(PROJECT_ID_COLUMN, PROJECT_FINGERPRINT_COLUMN);
	//		Unique index to ensure name+variant+version combinations are unique:
	static private final Index PROJECT_INDEX_UNIQUE = new Index("ProjectUnique", true, PROJECT_NAME_COLUMN, PROJECT_VARIANT_COLUMN, PROJECT_VERSION_COLUMN);
	//		Add columns and primary key to Project schema & seal it:
	static
	{
		PROJECT_SCHEMA.addColumn(PROJECT_ID_COLUMN);
		PROJECT_SCHEMA.addColumn(PROJECT_FINGERPRINT_COLUMN);
		PROJECT_SCHEMA.addColumn(PROJECT_NAME_COLUMN);
		PROJECT_SCHEMA.addColumn(PROJECT_VARIANT_COLUMN);
		PROJECT_SCHEMA.addColumn(PROJECT_VERSION_COLUMN);
		PROJECT_SCHEMA.addColumn(PROJECT_V1X_SCHEMA_VERSION_COLUMN);
		PROJECT_SCHEMA.setPrimaryKey(PROJECT_KEY);
		PROJECT_SCHEMA.addIndex(PROJECT_INDEX_UNIQUE);
		PROJECT_SCHEMA.seal(); // !!!
	}
	//	Held Foreign Key (HFK) schema: to store "held" foreign keys (RecordReferences) on Relationship fields
	static public final Schema HFK_SCHEMA = new Schema(COLLECTOR_MANAGEMENT_MODEL, "HeldForeignKey");
	//		Columns:
	static private final ForeignKeyColumn HFK_PROJECT_KEY_COLUMN = new ForeignKeyColumn(PROJECT_SCHEMA, false);
	static private final IntegerColumn HFK_FORM_POSITION_COLUMN = new IntegerColumn("formPosition", false, 0, Project.MAX_FORMS - 1);
	static private final IntegerColumn HFK_RELATIONSHIP_FIELD_POSITION_COLUMN = new IntegerColumn("relationshipFieldPosition", false, 0, Form.MAX_FIELDS - 1);
	static private final StringColumn HFK_SERIALISED_RECORD_REFERENCE = StringColumn.ForCharacterCount("serialisedRecordReference", false, 256);
	//		Primary key:
	static private final PrimaryKey HFK_KEY = PrimaryKey.WithColumnNames(HFK_PROJECT_KEY_COLUMN, HFK_FORM_POSITION_COLUMN, HFK_RELATIONSHIP_FIELD_POSITION_COLUMN);
	//		Add columns and primary key to HFK schema & seal it:
	static
	{
		HFK_SCHEMA.addColumn(HFK_PROJECT_KEY_COLUMN);
		HFK_SCHEMA.addColumn(HFK_FORM_POSITION_COLUMN);
		HFK_SCHEMA.addColumn(HFK_RELATIONSHIP_FIELD_POSITION_COLUMN);
		HFK_SCHEMA.addColumn(HFK_SERIALISED_RECORD_REFERENCE);
		HFK_SCHEMA.setPrimaryKey(HFK_KEY);
		HFK_SCHEMA.seal();
	}
			
	// DYNAMICS--------------------------------------------
	private final RecordStore recordStore;
	private final Map<Long, Project> cache;
	
	/**
	 * 
	 */
	public ProjectRecordStore(RecordStore recordStore)
	{
		this.recordStore = recordStore;
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
		return PROJECT_SCHEMA.createRecordReference(project.getID(), project.getFingerPrint());
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
		
		// TODO get project from cache or parse it
	//		File xmlFile = new File(folderPath.toString() + ProjectLoader.PROJECT_FILE);
	//		// Use the path where the xml file resides as the basePath (img&snd folders are assumed to be in the same place), no subfolders are created:
	//		ProjectParser parser = new ProjectParser(xmlFile.getParentFile().getAbsolutePath(), false);
	//		try
	//		{
	//			return parser.parseProject(xmlFile);
	//		}
	//		catch(Exception e)
	//		{
	//			e.printStackTrace();
	//		}
			
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
		return (long) projectID << 32 | projectFingerPrint & 0xFFFFFFFFL;
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

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#store(uk.ac.ucl.excites.sapelli.collector.model.Project)
	 */
	@Override
	public void store(Project project) throws DuplicateException
	{
		try
		{
			recordStore.store(getProjectRecord(project));
			cacheProject(project);
		}
		catch(DBConstraintException dbCE)
		{
			throw new DuplicateException("There is already a project with signature \"" + project.toString(false) + "\". Either delete the existing one or change the version of the new one.");
		}
		catch(DBException e)
		{
			e.printStackTrace();
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
			recordStore.delete(getProjectRecordReference(project));
			cache.remove(getCacheKey(project.getID(), project.getFingerPrint()));
		}
		catch(DBException e)
		{
			e.printStackTrace(System.err);
		}
	}
	
	private Record getHFKRecord(Relationship relationship, RecordReference foreignKey)
	{
		Form form = relationship.getForm();
		return HFK_SCHEMA.createRecord(	getProjectRecordReference(form.getProject()),
										form.getPosition(),
										form.getFieldPosition(relationship),
										foreignKey.serialise());
	}
	
	private RecordReference getHFKRecordReference(Relationship relationship)
	{
		Form form = relationship.getForm();
		return HFK_SCHEMA.createRecordReference(getProjectRecordReference(form.getProject()),
												form.getPosition(),
												form.getFieldPosition(relationship)); 
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
		Record hfkRecord = recordStore.retrieveRecord(getHFKRecordReference(relationship).getRecordQuery());
		if(hfkRecord != null)
		{
			try
			{
				return relationship.getRelatedForm().getSchema().createRecordReference(HFK_SERIALISED_RECORD_REFERENCE.retrieveValue(hfkRecord));
			}
			catch(Exception e)
			{
				deleteHeldForeignKey(relationship);
			}
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
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.shared.db.Store#finalise()
	 */
	@Override
	public void finalise() throws DBException
	{
		recordStore.finalise();
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.shared.db.Store#backup(java.io.File)
	 */
	@Override
	public void backup(File destinationFolder) throws DBException
	{
		recordStore.backup(destinationFolder);
	}

}
