/**
 * 
 */
package uk.ac.ucl.excites.storage.db;

import java.util.List;

import uk.ac.ucl.excites.storage.model.Schema;
import android.util.Log;

import com.db4o.Db4oEmbedded;
import com.db4o.ObjectContainer;

/**
 * @author mstevens
 * 
 */
public final class DataAccess {

	static private String TAG = "DATA ACCESS";
	private ObjectContainer db;

	static private DataAccess INSTANCE = null;

	static public DataAccess getInstance(String dbFilePath) {
		if (INSTANCE == null)
			INSTANCE = new DataAccess(dbFilePath);
		return INSTANCE;
	}

	// private DB4O ...

	private DataAccess(String dbFilePath) {
		try {
			if (db == null || db.ext().isClosed()) {
				this.db = Db4oEmbedded.openFile(
						Db4oEmbedded.newConfiguration(), dbFilePath);
				Log.d(TAG, "opened new database connection");
			}

		} catch (Exception e) {
			Log.e(TAG, "unable to open database");
		}
	}

	// TODO access methods
	
	// store schema
	public void store(Schema schema) {
		db.store(schema);
	}
	
	// retrieve all schemata
	public List<Schema> retrieveSchemata() {
		List<Schema> result = db.query(Schema.class);
		return result;
	}

}
