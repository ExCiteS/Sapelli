package uk.ac.ucl.excites.collector;

import java.io.File;

import uk.ac.ucl.excites.collector.project.db.DataAccess;
import uk.ac.ucl.excites.util.Debug;
import android.app.Application;
import android.content.res.Configuration;

import com.db4o.Db4oEmbedded;
import com.db4o.ObjectContainer;
import com.db4o.config.EmbeddedConfiguration;
import com.db4o.ext.DatabaseFileLockedException;
import com.db4o.ext.DatabaseReadOnlyException;
import com.db4o.ext.Db4oIOException;
import com.db4o.ext.IncompatibleFileFormatException;
import com.db4o.ext.OldFormatException;

/**
 * Application App to keep the db4o object throughout the lifecycle of the Collector
 * 
 * @author Michalis Vitos
 * 
 */
public class CollectorApp extends Application
{
	static private final String DATABASE_NAME = "ExCiteS.db4o";

	private volatile static ObjectContainer db;
	private EmbeddedConfiguration dbConfig;

	@Override
	public void onConfigurationChanged(Configuration newConfig)
	{
		super.onConfigurationChanged(newConfig);
		Debug.d(newConfig.toString());
	}

	@Override
	public void onCreate()
	{
		super.onCreate();
		Debug.d("Called!");

		// TODO Create the db Container
		dbInit();
	}

	@Override
	public void onLowMemory()
	{
		super.onLowMemory();
		Debug.d("onLowMemory() called!");
	}

	@Override
	public void onTerminate()
	{
		super.onTerminate();
		// This method is for use in emulated process environments. It will never be called on
		// a production Android device, where processes are removed by simply killing them; no
		// user code (including this callback) is executed when doing so.
		Debug.d("Should never be called!");
	}

	private void dbInit()
	{
		String dbFileName = getDatabasePath();

		if(dbFileName == null || dbFileName.isEmpty())
			throw new IllegalArgumentException("Invalid database file name");
		try
		{
			dbConfig = Db4oEmbedded.newConfiguration();
			dbConfig.common().exceptionsOnNotStorable(true);
			openDB(dbFileName); // open the database! (throws various exceptions)
		}
		catch(Exception e)
		{
			Debug.e("Unable to open database.", e);
		}
	}

	/**
	 * (Re)Opens the database
	 */
	private void openDB(String dbFileName) throws Db4oIOException, DatabaseFileLockedException, IncompatibleFileFormatException, OldFormatException, DatabaseReadOnlyException
	{
		if(db != null)
		{
			Debug.i("Database is already open.");
			return;
		}
		db = Db4oEmbedded.openFile(Db4oEmbedded.newConfiguration(), dbFileName);
		Debug.i("Opened new database connection in file: " + dbFileName);
	}

	public DataAccess getDatabaseInstance()
	{
		DataAccess dao = new DataAccess(db);
		return dao;
	}

	public String getDatabasePath()
	{
		// Always store the db to the internal storage of the Android device
		return getFilesDir().getAbsolutePath() + File.separator + DATABASE_NAME;
	}

	// TODO
	// public void closeDB()
	// {
	// db.close();
	// db = null;
	// System.out.println(" closed database connection");
	// }
	//
	// public boolean isOpen()
	// {
	// return db != null;
	// }

}
