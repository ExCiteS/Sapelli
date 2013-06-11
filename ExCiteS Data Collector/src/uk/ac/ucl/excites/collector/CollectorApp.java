package uk.ac.ucl.excites.collector;

import java.io.File;
import java.util.HashSet;
import java.util.Set;

import uk.ac.ucl.excites.collector.database.DataAccess;
import uk.ac.ucl.excites.collector.database.DataAccessClient;
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

/**
 * Application App to keep the db4o object throughout the lifecycle of the Collector
 * 
 * @author Michalis Vitos, mstevens
 * 
 */
public class CollectorApp extends Application
{
	
	static private final String EXCITES_FOLDER = "ExCiteS" + File.separatorChar;
	static private final String DATABASE_NAME = "ExCiteS.db4o";
	static private final String DUMP_FOLDER = "Dumps" + File.separator;

	static private volatile ObjectContainer db;
	
	private String excitesFolderPath;
	private String dumpFolderPath;

	private Set<DataAccessClient> daoClients;
	
	@Override
	public void onConfigurationChanged(Configuration newConfig)
	{
		super.onConfigurationChanged(newConfig);
		// Debug.d(newConfig.toString());
	}

	@Override
	public void onCreate()
	{
		super.onCreate();
		Debug.d("Called!");
	
		// Db clients:
		daoClients = new HashSet<DataAccessClient>();
		
		// Paths:
		excitesFolderPath = Environment.getExternalStorageDirectory().getAbsolutePath() + File.separator + EXCITES_FOLDER;
		dumpFolderPath =  excitesFolderPath + DUMP_FOLDER;
		
		// Set up a CrashReporter to the ExCiteS/crash Folder
		Thread.setDefaultUncaughtExceptionHandler(new CrashReporter(dumpFolderPath, getResources().getString(R.string.app_name)));
	}

	/**
	 * @return the excitesFolderPath
	 */
	public String getExcitesFolderPath()
	{
		return excitesFolderPath;
	}

	/**
	 * @return the dumpFolderPath
	 */
	public String getDumpFolderPath()
	{
		return dumpFolderPath;
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
	private boolean openDB()
	{
		try
		{		
			if(db != null)
			{
				Debug.i("Database is already open.");
				return true;
			}
			// Configure the db:
			EmbeddedConfiguration dbConfig = Db4oEmbedded.newConfiguration();
			dbConfig.common().updateDepth(DataAccess.UPDATE_DEPTH);
			dbConfig.common().exceptionsOnNotStorable(true);
			dbConfig.common().objectClass(Record.class).cascadeOnActivate(true);
			dbConfig.common().objectClass(Record.class).cascadeOnUpdate(true);
			dbConfig.common().objectClass(Transmission.class).cascadeOnActivate(true);
			dbConfig.common().objectClass(Transmission.class).cascadeOnUpdate(true);
			dbConfig.common().objectClass(Project.class).cascadeOnActivate(true);
			dbConfig.common().objectClass(Project.class).cascadeOnUpdate(true);
			// Open the db:
			db = Db4oEmbedded.openFile(dbConfig, getDatabasePath()); // (throws various exceptions)
			Debug.i("Opened new database connection in file: " + getDatabasePath());
			return true;
		}
		catch(Exception e)
		{
			Debug.e("Unable to open database.", e);
			db = null;
			return false;
		}		
	}

	/**
	 * Called by a DataAccessClient to request a DataAccess object
	 * 
	 * @param client
	 * @return
	 */
	public DataAccess getDataAccess(DataAccessClient client)
	{
		if(db == null)
		{
			if(!openDB())
				return null;
		}
		daoClients.add(client);
		return new DataAccess(db);
	}
	
	/**
	 * Called by a DataAccessClient to signal it will no longer use its DataAccess object 
	 * 
	 * @param client
	 */
	public void discardDataAccess(DataAccessClient client)
	{
		daoClients.remove(client);
		if(daoClients.isEmpty() && db != null)
		{
			db.close();
			db = null;
			Debug.i("Closed database connection");
		}
	}

	/**
	 * Always store the db to the internal storage of the Android device
	 * 
	 * @return
	 */
	public String getDatabasePath()
	{
		return getFilesDir().getAbsolutePath() + File.separator + DATABASE_NAME;
	}
	
	public void backupDatabase(String filePath)
	{
		db.commit();
		db.ext().backup(filePath);
	}

}
