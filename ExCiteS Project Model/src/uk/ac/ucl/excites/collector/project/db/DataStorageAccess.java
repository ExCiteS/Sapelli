/**
 * 
 */
package uk.ac.ucl.excites.collector.project.db;

import java.util.List;

import uk.ac.ucl.excites.storage.model.Schema;
import android.util.Log;

import com.db4o.Db4oEmbedded;
import com.db4o.ObjectContainer;
import com.db4o.query.Predicate;

/**
 * @author mstevens, julia
 * 
 */
public final class DataStorageAccess
{

	static private String TAG = "DATA ACCESS";
	private ObjectContainer db;

	static private DataStorageAccess INSTANCE = null;

	static public DataStorageAccess getInstance(String dbFilePath)
	{
		if(INSTANCE == null)
			INSTANCE = new DataStorageAccess(dbFilePath);
		return INSTANCE;
	}

	// private DB4O ...

	private DataStorageAccess(String dbFilePath)
	{
		try
		{
			if(db == null || db.ext().isClosed())
			{
				this.db = Db4oEmbedded.openFile(Db4oEmbedded.newConfiguration(), dbFilePath);
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
	
//	/**
//	 * @param project
//	 */
//	public void store(Project project)
//	{
//		db.store(project);
//	}
//
//	/**
//	 * Retrieves all schemata
//	 * 
//	 * @return
//	 */
//	public List<Schema> retrieveSchemata()
//	{
//		List<Schema> result = db.query(Schema.class);
//		return result;
//	}
//	
	

}
