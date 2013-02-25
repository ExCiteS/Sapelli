/**
 * 
 */
package uk.ac.ucl.excites.collector.project.db;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.util.DuplicateException;
import uk.ac.ucl.excites.storage.model.Schema;
import android.util.Log;

import com.db4o.Db4oEmbedded;
import com.db4o.ObjectContainer;
import com.db4o.ObjectSet;
import com.db4o.query.Predicate;

/**
 * @author mstevens, julia
 * 
 */
public final class DataAccess
{

	static private final String TAG = "DATA ACCESS";
	static private final int PROJECT_ACTIVATION_DEPTH = 500;
	static private final String DB_FILE_NAME = "ExCiteS.db4o";

	private ObjectContainer db;

	static private DataAccess INSTANCE = null;

	static public DataAccess getInstance(String dbFolderPath)
	{
		if(INSTANCE == null)
			INSTANCE = new DataAccess(dbFolderPath);
		return INSTANCE;
	}

	// private DB4O ...

	private DataAccess(String dbFolderPath)
	{
		try
		{
			if(db == null || db.ext().isClosed())
			{
				this.db = Db4oEmbedded.openFile(Db4oEmbedded.newConfiguration(), dbFolderPath + File.separator + DB_FILE_NAME);
				Log.d(TAG, "Opened new database connection");
			}
		}
		catch(Exception e)
		{
			Log.e(TAG, "Unable to open database");
		}
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
		if(retrieveProject(project.getName()) != null)
			throw new DuplicateException("There is already a project named \"" + project.getName() + "\"!");
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
	public Project retrieveProject(String projectName)
	{
		Project theExample = new Project(projectName);
		final List<Project> result = db.queryByExample(theExample);
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

	/**
	 * Close db
	 * 
	 * @return
	 */
	public void closeDB()
	{
		db.close();
		INSTANCE = null;
		Log.d(TAG, "Closed database connection");
	}

}
