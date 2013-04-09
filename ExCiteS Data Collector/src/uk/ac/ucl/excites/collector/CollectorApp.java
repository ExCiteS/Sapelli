package uk.ac.ucl.excites.collector;

import java.io.File;

import uk.ac.ucl.excites.collector.database.DataAccess;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.util.CrashReporter;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.transmission.Transmission;
import uk.ac.ucl.excites.util.Debug;
import android.app.Application;
import android.content.res.Configuration;
import android.os.Environment;

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
	private static final String DATABASE_NAME = "ExCiteS.db4o";
	private static final String CRASH_FOLDER = "ExCiteS" + File.separator + "crash";

	private volatile static ObjectContainer db;

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
		
		// Set up a CrashReporter to the ExCiteS/crash Folder
		final String localPath = Environment.getExternalStorageDirectory().getAbsolutePath() + File.separator + CRASH_FOLDER;
		Thread.setDefaultUncaughtExceptionHandler(new CrashReporter(localPath, getResources().getString(R.string.app_name)));

		String dbFileName = getDatabasePath();
		try
		{
			EmbeddedConfiguration dbConfig = Db4oEmbedded.newConfiguration();
			dbConfig.common().updateDepth(DataAccess.UPDATE_DEPTH);
			dbConfig.common().exceptionsOnNotStorable(true);
			dbConfig.common().objectClass(Record.class).cascadeOnActivate(true);
			dbConfig.common().objectClass(Record.class).cascadeOnUpdate(true);
			dbConfig.common().objectClass(Transmission.class).cascadeOnActivate(true);
			dbConfig.common().objectClass(Transmission.class).cascadeOnUpdate(true);
			dbConfig.common().objectClass(Project.class).cascadeOnActivate(true);
			dbConfig.common().objectClass(Project.class).cascadeOnUpdate(true);
			openDB(dbFileName, dbConfig); // open the database! (throws various exceptions)
		}
		catch(Exception e)
		{
			Debug.e("Unable to open database.", e);
		}
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

	/**
	 * (Re)Opens the database
	 */
	private void openDB(String dbFileName, EmbeddedConfiguration config) throws Db4oIOException, DatabaseFileLockedException, IncompatibleFileFormatException, OldFormatException, DatabaseReadOnlyException
	{
		if(db != null)
		{
			Debug.i("Database is already open.");
			return;
		}
		db = Db4oEmbedded.openFile(config, dbFileName);
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
	
	public void backupDatabase(String filePath)
	{
		db.commit();
		db.ext().backup(filePath);
	}
	
	// TODO close?
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
