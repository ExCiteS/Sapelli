/**
 * 
 */
package uk.ac.ucl.excites.collector.project.db;

import java.io.File;
import java.util.List;

import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.util.DuplicateException;
import uk.ac.ucl.excites.collector.project.util.FileHelpers;
import uk.ac.ucl.excites.storage.model.Schema;
import android.util.Log;

import com.db4o.Db4oEmbedded;
import com.db4o.ObjectContainer;
import com.db4o.ObjectSet;
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
	static private final int PROJECT_ACTIVATION_DEPTH = 100;
	static private DataAccess INSTANCE = null;

	static public DataAccess getInstance(String dbFolderPath)
	{
		if(INSTANCE == null || INSTANCE.dbFolderPath != dbFolderPath)
			INSTANCE = new DataAccess(dbFolderPath);
		return INSTANCE;
	}

	//Dynamics---------------------------------------------
	private String dbFolderPath;
	private ObjectContainer db;
	
	private DataAccess(String dbFolderPath)
	{
		if(dbFolderPath == null || dbFolderPath.isEmpty())
			throw new IllegalArgumentException("Invalid database folder path");
		this.dbFolderPath = dbFolderPath;
		try
		{
			openDB(); //open the database!
			Log.d(TAG, "Opened new database connection in file: " + getDbPath());
		}
		catch(Exception e)
		{
			Log.e(TAG, "Unable to open database");
		}
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
		if(db != null)
		{
			//Log.w(TAG, "Database is already open.");
			return;
		}
		this.db = Db4oEmbedded.openFile(Db4oEmbedded.newConfiguration(), getDbPath());
	}
	
	/**
	 * Closes the database. IT can be reopend with openDB().
	 */
	public void closeDB()
	{
		db.close();
		db = null;
		Log.d(TAG, "Closed database connection");
	}
	
	public boolean isOpen()
	{
		return db != null;
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
	public void copyDBtoSD(String dstFilePath)
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
		return result;
	}

	/**
	 * @param id
	 * @param version
	 * @return
	 */
	@SuppressWarnings("serial")
	public Schema retrieveSchema(final int id, final int version)
	{
		ObjectSet<Schema> result = db.query(new Predicate<Schema>()
		{
			public boolean match(Schema schema)
			{
				return schema.getID() == id && schema.getVersion() == version;
			}
		});
		if(result.hasNext())
			return result.next();
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
	 * Retrieves all projects
	 * 
	 * @return
	 */
	public List<Project> retrieveProjects()
	{
		final List<Project> result = db.queryByExample(Project.class);
		for(Project p : result)
			db.activate(p, PROJECT_ACTIVATION_DEPTH);
		return result;
	}

	/**
	 * Retrieves specific Project
	 * 
	 * @return null if project was not found
	 */
	public Project retrieveProject(final String name, final int version)
	{
		@SuppressWarnings("serial")
		ObjectSet<Project> result = db.query(new Predicate<Project>()
		{
			public boolean match(Project project)
			{
				return project.getName().equalsIgnoreCase(name) && project.getVersion() == version;
			}
		});
		if(result.isEmpty())
			return null;
		else
		{
			Project p = result.get(0);
			db.activate(p, PROJECT_ACTIVATION_DEPTH);
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
	
}
