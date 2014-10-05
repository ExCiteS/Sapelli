/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.db;

import java.io.File;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.SapelliCollectorClient;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Relationship;
import uk.ac.ucl.excites.sapelli.collector.util.DuplicateException;
import uk.ac.ucl.excites.sapelli.shared.db.DBException;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.PrimaryKey;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.RecordReference;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;

/**
 * @author mstevens
 *
 */
public class ProjectRecordStore extends ProjectStore
{
	// STATICS---------------------------------------------
	// Project storage model:
	//	Model:
	static public final Model COLLECTOR_MANAGEMENT_MODEL = new Model(SapelliCollectorClient.COLLECTOR_MANAGEMENT_MODEL_ID, "CollectorManagement");
	//	 Project schema:
	static public final Schema PROJECT_SCHEMA = new Schema(COLLECTOR_MANAGEMENT_MODEL, "Project");
	//		Columns:
	static private final IntegerColumn PROJECT_ID_COLUMN = new IntegerColumn("id", false, Project.PROJECT_ID_FIELD);
	static private final IntegerColumn PROJECT_FINGERPRINT_COLUMN = new IntegerColumn("fingerprint", false, Project.PROJECT_FINGERPRINT_FIELD);
	static private final StringColumn PROJECT_NAME_COLUMN = StringColumn.ForCharacterCount("name", false, 128);
	static private final StringColumn PROJECT_VARIANT_COLUMN = StringColumn.ForCharacterCount("variant", true, 128);
	static private final StringColumn PROJECT_VERSION_COLUMN = StringColumn.ForCharacterCount("version", false, 32);
	static private final BooleanColumn PROJECT_V1X_COLUMN = new BooleanColumn("v1x", false);
	//		Primary key:
	static private final PrimaryKey PROJECT_KEY = PrimaryKey.WithColumnNames(PROJECT_ID_COLUMN, PROJECT_FINGERPRINT_COLUMN);
	//		Add columns and primary key to Project schema & seal it:
	static
	{
		PROJECT_SCHEMA.addColumn(PROJECT_ID_COLUMN);
		PROJECT_SCHEMA.addColumn(PROJECT_FINGERPRINT_COLUMN);
		PROJECT_SCHEMA.addColumn(PROJECT_NAME_COLUMN);
		PROJECT_SCHEMA.addColumn(PROJECT_VARIANT_COLUMN);
		PROJECT_SCHEMA.addColumn(PROJECT_VERSION_COLUMN);
		PROJECT_SCHEMA.addColumn(PROJECT_V1X_COLUMN);
		PROJECT_SCHEMA.setPrimaryKey(PROJECT_KEY);
		PROJECT_SCHEMA.seal(); // !!!
	}
	//	Held Foreign Key (HFK) schema: to store "held" foreign keys (RecordReferences) on Relationship fields
	static public final Schema HFK_SCHEMA = new Schema(COLLECTOR_MANAGEMENT_MODEL, "HeldForeignKey");
	//		Columns:
	static private final ForeignKeyColumn HFK_PROJECT_KEY_COLUMN = new ForeignKeyColumn("project", PROJECT_SCHEMA, false);
	static private final IntegerColumn HFK_FORM_POSITION_COLUMN = new IntegerColumn("formPosition", false, 0, Project.MAX_FORMS - 1);
	static private final IntegerColumn HFK_RELATIONSHIP_FIELD_POSITION_COLUMN = new IntegerColumn("relationshipFieldPosition", false, 0, Form.MAX_FIELDS - 1);
	static private final StringColumn HFK_SERIALISED_RECORD_REFERENCE = StringColumn.ForCharacterCount("serialisedRecordReference", false, 256);
	//		Primary key:
	static private final PrimaryKey HFK_KEY = PrimaryKey.WithColumnNames(PROJECT_ID_COLUMN, PROJECT_FINGERPRINT_COLUMN);
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
	
	/**
	 * 
	 */
	public ProjectRecordStore(RecordStore recordStore)
	{
		this.recordStore = recordStore;
	}
	
	private Record getProjectRecord(Project project)
	{
		Record projRec = PROJECT_SCHEMA.createRecord();
		PROJECT_ID_COLUMN.storeValue(projRec, project.getID());
		PROJECT_FINGERPRINT_COLUMN.storeValue(projRec, project.getFingerPrint());
		PROJECT_NAME_COLUMN.storeValue(projRec, project.getName());
		PROJECT_VARIANT_COLUMN.storeValue(projRec, project.getVariant());
		PROJECT_VERSION_COLUMN.storeValue(projRec, project.getVersion());
		PROJECT_V1X_COLUMN.storeValue(projRec, project.isV1xProject());
		return projRec;
	}
	
	private Project getProject(Record projRec)
	{
		// TODO
		return null;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#retrieveV1Project(int, int)
	 */
	@Override
	public Project retrieveV1Project(int schemaID, int schemaVersion)
	{
		// TODO Auto-generated method stub
		
		
		
		return null;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#store(uk.ac.ucl.excites.sapelli.collector.model.Project)
	 */
	@Override
	public void store(Project project) throws DuplicateException
	{
		// TODO how to check for duplicates?
		try
		{
			recordStore.store(getProjectRecord(project));
		}
		catch(DBException e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#retrieveProjects()
	 */
	@Override
	public List<Project> retrieveProjects()
	{
		return null;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#retrieveProject(java.lang.String, java.lang.String, java.lang.String)
	 */
	@Override
	public Project retrieveProject(String name, String variant, String version)
	{
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#retrieveProject(int, int)
	 */
	@Override
	public Project retrieveProject(int projectID, int projectFingerPrint)
	{
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#retrieveProjectVersions(int)
	 */
	@Override
	public List<Project> retrieveProjectVersions(int projectID)
	{
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#delete(uk.ac.ucl.excites.sapelli.collector.model.Project)
	 */
	@Override
	public void delete(Project project)
	{
		try
		{
			recordStore.delete(getProjectRecord(project));
		}
		catch(DBException e)
		{
			e.printStackTrace();
		}
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#storeHeldForeignKey(uk.ac.ucl.excites.sapelli.collector.model.fields.Relationship, uk.ac.ucl.excites.sapelli.storage.model.RecordReference)
	 */
	@Override
	public void storeHeldForeignKey(Relationship relationship, RecordReference foreignKey)
	{
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#retrieveHeldForeignKey(uk.ac.ucl.excites.sapelli.collector.model.fields.Relationship)
	 */
	@Override
	public RecordReference retrieveHeldForeignKey(Relationship relationship)
	{
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#deleteHeldForeignKey(uk.ac.ucl.excites.sapelli.collector.model.fields.Relationship)
	 */
	@Override
	public void deleteHeldForeignKey(Relationship relationship)
	{
		// TODO Auto-generated method stub

	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.shared.db.Store#finalise()
	 */
	@Override
	public void finalise() throws DBException
	{
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.shared.db.Store#backup(java.io.File)
	 */
	@Override
	public void backup(File destinationFolder) throws DBException
	{
		// TODO Auto-generated method stub

	}

}
