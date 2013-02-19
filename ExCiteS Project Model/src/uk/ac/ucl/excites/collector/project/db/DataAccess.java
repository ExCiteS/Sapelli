/**
 * 
 */
package uk.ac.ucl.excites.collector.project.db;

import java.util.List;

import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.util.DuplicateException;
import uk.ac.ucl.excites.storage.model.Schema;
import android.util.Log;

import com.db4o.Db4oEmbedded;
import com.db4o.ObjectContainer;
import com.db4o.query.Predicate;

/**
 * @author mstevens, julia
 * 
 */
public final class DataAccess
{

	static private String TAG = "DATA ACCESS";
	private ObjectContainer db;

	static private DataAccess INSTANCE = null;

	static public DataAccess getInstance(String dbFilePath)
	{
		if(INSTANCE == null)
			INSTANCE = new DataAccess(dbFilePath);
		return INSTANCE;
	}

	// private DB4O ...

	private DataAccess(String dbFilePath)
	{
		try
		{
			if(db == null || db.ext().isClosed())
			{
				this.db = Db4oEmbedded.openFile(Db4oEmbedded.newConfiguration(), dbFilePath + "/ExCiteS.db4o");
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
	@SuppressWarnings("serial")
	public Schema retrieveSchema(final int id, final int version)
	{
		return db.query(new Predicate<Schema>()
		{
			public boolean match(Schema s)
			{
				return s.getID() == id && s.getVersion() == version;
			}
		}).next(); // TODO check if this will return null if no match is find (rather than throwing an exception)

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
		return result;
	}
	
	public Project retrieveProject(String projectName)
	{
		//TODO
		return null;
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
