/**
 * 
 */
package uk.ac.ucl.excites.collector.project.db;

import java.io.File;
import java.util.List;

import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.util.DuplicateException;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.transmission.Transmission;
import uk.ac.ucl.excites.transmission.sms.SMSTransmission;
import uk.ac.ucl.excites.util.FileHelpers;

import com.db4o.Db4oEmbedded;
import com.db4o.ObjectContainer;
import com.db4o.ObjectSet;
import com.db4o.config.EmbeddedConfiguration;
import com.db4o.query.Predicate;


/**
 * @author mstevens, julia, Michalis Vitos
 * 
 */
public final class DataAccess
{

	//Statics----------------------------------------------
	static private final String TAG = "DATA ACCESS";
	static private final String DATABASE_NAME = "ExCiteS.db4o";
	static private final int ACTIVATION_DEPTH = 100;
	static private DataAccess INSTANCE = null;

	static public DataAccess getInstance(String dbFolderPath)
	{
		if(INSTANCE == null || INSTANCE.dbFolderPath != dbFolderPath)
			INSTANCE = new DataAccess(dbFolderPath);
		INSTANCE.openDB();
		return INSTANCE;
	}

	static public DataAccess getInstance(ObjectContainer db)
	{
		return new DataAccess(db);
	}

	//Dynamics---------------------------------------------
	private String dbFolderPath = null;
	private boolean serverMode = false;
	private ObjectContainer db;
	
	private DataAccess(String dbFolderPath)
	{
		if(dbFolderPath == null || dbFolderPath.isEmpty())
			throw new IllegalArgumentException("Invalid database folder path");
		this.dbFolderPath = dbFolderPath;
		try
		{
			openDB(); //open the database!
			System.out.println(TAG + " opened new database connection in file: " + getDbPath());
		}
		catch(Exception e)
		{
			System.err.println(TAG + " is unable to open database.");
			e.printStackTrace(System.err);
		}
	}
	
	private DataAccess(ObjectContainer db)
	{
		this.db = db;
		this.serverMode = true;
	}

	/**
	 * @return the dbFolderPath
	 */
	public String getDbFolderPath()
	{
		return dbFolderPath;
	}

	/**
	 * (Re)Opens the database
	 */
	public void openDB()
	{
		if(serverMode || db != null)
		{
			//Log.w(TAG, "Database is already open.");
			return;
		}
		EmbeddedConfiguration dbConfig = Db4oEmbedded.newConfiguration();
		dbConfig.common().exceptionsOnNotStorable(true);
		this.db = Db4oEmbedded.openFile(dbConfig, getDbPath());		
	}
	
	/**
	 * Closes the database. IT can be reopend with openDB().
	 */
	public void closeDB()
	{
		if(db != null && !serverMode) //never close in servermode!
		{
			db.close();
			db = null;
			System.out.println(TAG + " closed database connection");
		}
	}
	
	public boolean isOpen()
	{
		return db != null;
	}
	
	public void commit()
	{
		if(db != null)
			db.commit();
	}

	/**
	 * Returns the file where the DB is saved
	 * 
	 * @return
	 */
	public String getDbPath()
	{
		return dbFolderPath + File.separator + DATABASE_NAME;
	}

	/**
	 * Copy Database File to the destination
	 * 
	 * @param dstFilePath
	 */
	public void copyDB(String dstFilePath)
	{
		FileHelpers.copyFile(getDbPath(), dstFilePath);
	}

	/**
	 * @param schema
	 */
	public void store(Schema schema)
	{
		db.store(schema);
	}

	/**
	 * Retrieves all schemata
	 * 
	 * @return
	 */
	public List<Schema> retrieveSchemata()
	{
		List<Schema> result = db.query(Schema.class);
		for(Schema s : result)
			db.activate(s, ACTIVATION_DEPTH);
		return result;
	}
	
	/**
	 * @param record
	 */
	public void store(Record record)
	{
		db.store(record);
	}
	
	/**
	 * Retrieve Records by their Schema
	 * 
	 * @param schema
	 * @return
	 */
	public List<Record> retrieveRecords(final Schema schema)
	{
		ObjectSet<Record> result = db.query(new Predicate<Record>()
		{
			private static final long serialVersionUID = 1L;
			
			public boolean match(Record record)
			{
				return record.getSchema() == schema;
			}
		});
		for(Record r : result)
			db.activate(r, ACTIVATION_DEPTH);
		return result;
	}
	
	public List<Record> retrieveRecordsWithoutTransmission(final Schema schema)
	{
		return retrieveRecords(schema, null);
	}
	
	/**
	 * Retrieve Records of a given Schema which are associated with a given transmission
	 * 
	 * @param schema
	 * @return
	 */
	public List<Record> retrieveRecords(final Schema schema, final Transmission transmission)
	{
		ObjectSet<Record> result = db.query(new Predicate<Record>()
		{
			private static final long serialVersionUID = 1L;
			
			public boolean match(Record record)
			{
				return record.getSchema() == schema && record.getTransmission() == transmission;
			}
		});
		for(Record r : result)
			db.activate(r, ACTIVATION_DEPTH);
		return result;
	}
	
	/**
	 * @param id
	 * @param version
	 * @return
	 */
	public Schema retrieveSchema(final int id, final int version)
	{
		ObjectSet<Schema> result = db.query(new Predicate<Schema>()
		{
			private static final long serialVersionUID = 1L;
			
			public boolean match(Schema schema)
			{
				return schema.getID() == id && schema.getVersion() == version;
			}
		});
		if(result.hasNext())
		{
			Schema s = result.next();
			db.activate(s, ACTIVATION_DEPTH);
			return s;
		}
		else
			return null;
	}
	
	/**
	 * @param project
	 */
	public void store(Project project) throws DuplicateException
	{
		if(retrieveProject(project.getName(), project.getVersion()) != null)
			throw new DuplicateException("There is already a project named \"" + project.getName() + "\", with version " + project.getVersion() + ". Either remove the existing one or increment the version of the new one.");
		db.store(project);
	}
	
	/**
	 * @param project
	 */
	public void update(Project project)
	{
		db.store(project);
	}

	/**
	 * Retrieves all projects
	 * 
	 * @return
	 */
	public List<Project> retrieveProjects()
	{
		final List<Project> result = db.queryByExample(Project.class);
		for(Project p : result)
			db.activate(p, ACTIVATION_DEPTH);
		return result;
	}

	/**
	 * Retrieves specific Project
	 * 
	 * @return null if project was not found
	 */
	public Project retrieveProject(final String name, final String version)
	{
		@SuppressWarnings("serial")
		ObjectSet<Project> result = db.query(new Predicate<Project>()
		{
			public boolean match(Project project)
			{
				return project.getName().equalsIgnoreCase(name) && project.getVersion().equalsIgnoreCase(version);
			}
		});
		if(result.isEmpty())
			return null;
		else
		{
			Project p = result.get(0);
			db.activate(p, ACTIVATION_DEPTH);
			return p;
		}
	}

	/**
	 * Delete specific project
	 * 
	 * @return
	 */
	public void deleteProject(Project project)
	{
		db.delete(project);
	}
	
	/**
	 * Retrieve a form by its schemaID & version
	 * 
	 * @param schemaID
	 * @param schemaVersion
	 * @return
	 */
	public Form retrieveForm(final int schemaID, final int schemaVersion)
	{
		ObjectSet<Form> result = db.query(new Predicate<Form>()
		{
			private static final long serialVersionUID = 1L;
			
			public boolean match(Form form)
			{
				return form.getSchemaID() == schemaID && form.getSchemaVersion() == schemaVersion;
			}
		});
		if(result.hasNext())
		{
			Form f = result.next();
			db.activate(f, ACTIVATION_DEPTH);
			return f;
		}
		else
			return null;
	}
	
	/**
	 * @param transmission
	 */
	public void store(Transmission transmission)
	{
		db.store(transmission);
	}
	
	/**
	 * Retrieves all transmissions
	 * 
	 * @return
	 */
	public List<Transmission> retrieveTransmissions()
	{
		final List<Transmission> result = db.queryByExample(Transmission.class);
		for(Transmission t : result)
			db.activate(t, ACTIVATION_DEPTH);
		return result;
	}

	/**
	 * Retrieves all unsent transmissions
	 * 
	 * @return
	 */
	public List<Transmission> retrieveUnsentTransmissions()
	{
		@SuppressWarnings("serial")
		ObjectSet<Transmission> result = db.query(new Predicate<Transmission>()
		{
			public boolean match(Transmission t)
			{
				return !t.isSent();
			}
		});
		for(Transmission t : result)
			db.activate(t, ACTIVATION_DEPTH);
		return result;
	}
	
	/**
	 * Retrieve SMSTransmission by id
	 * 
	 * @param id
	 * @return
	 */
	public SMSTransmission retrieveSMSTransmission(final int id)
	{
		@SuppressWarnings("serial")
		ObjectSet<SMSTransmission> result = db.query(new Predicate<SMSTransmission>()
		{
			public boolean match(SMSTransmission t)
			{
				return t.getID() == id;
			}
		});
		if(result.isEmpty())
			return null;
		else
		{
			SMSTransmission t = result.get(0);
			db.activate(t, ACTIVATION_DEPTH);
			return t;
		}		
	}
	
}
