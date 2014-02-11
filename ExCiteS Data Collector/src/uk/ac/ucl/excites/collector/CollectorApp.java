package uk.ac.ucl.excites.collector;

import java.io.File;
import java.util.HashSet;
import java.util.Set;

import uk.ac.ucl.excites.collector.database.DB4OConnector;
import uk.ac.ucl.excites.collector.database.DataAccess;
import uk.ac.ucl.excites.collector.database.DataAccessClient;
import uk.ac.ucl.excites.collector.util.CrashReporter;
import uk.ac.ucl.excites.util.Debug;
import uk.ac.ucl.excites.util.io.FileHelpers;
import android.app.Application;
import android.content.res.Configuration;
import android.os.Environment;
import android.util.Log;

import com.db4o.ObjectContainer;

import de.jockels.open.Environment2;

/**
 * Application App to keep the db4o object throughout the life-cycle of the Collector
 * 
 * @author Michalis Vitos, mstevens
 * 
 */
public class CollectorApp extends Application
{
	
	static private final String TAG = "CollectorApp";
	
	static private final String SAPELLI_FOLDER = "Sapelli" + File.separatorChar;
	static private final String DATABASE_NAME = "Sapelli.db4o";
	static private final String DEMO_PREFIX = "Demo_";
	static private final String PROJECT_FOLDER = "Projects" + File.separator;
	static private final String TEMP_FOLDER = "Temp" + File.separator;
	static private final String DOWNLOAD_FOLDER = "Downloads" + File.separator;
	static private final String DUMP_FOLDER = "Dumps" + File.separator;

	static private volatile ObjectContainer db;
	
	private File sapelliFolder;

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
		Debug.d("CollectorApp started.\nBuild info:\n" + BuildInfo.getAllInfo());
	
		// Db clients:
		daoClients = new HashSet<DataAccessClient>();
		
		// Sapelli folder (created on SD card or internal mass storage):
		sapelliFolder = new File(getStorageDirectory().getAbsolutePath() + File.separator + SAPELLI_FOLDER);
		
		// Set up a CrashReporter to the Sapelli/crash Folder
		try
		{
			Thread.setDefaultUncaughtExceptionHandler(new CrashReporter(getDumpFolderPath(), getResources().getString(R.string.app_name)));
		}
		catch(Exception e)
		{
			Log.e(TAG, "Could not set-up DefaultUncaughtExceptionHandler", e);
		}
	}
	
	/**
	 * Uses Environment2 library to get the path of the actual SD card if there is one,
	 * if not it gets the path of the emulated SD card/internal mass storage
	 * 
	 * @return the directory as a file object
	 */
	public File getStorageDirectory()
	{
		return Environment2.getCardDirectory();
	}
	
	/**
	 * Uses Environment2 library to check whether the directory returned by getStorageDirectory() is on
	 * an accessible (i.e. mounted) storage device
	 * 
	 * @return
	 */
	static public boolean isStorageMounted()
	{
		if(Environment.MEDIA_MOUNTED.equals(Environment2.getCardState()))
			return true;
		return false;
	}

	/**
	 * @return creates the Sapelli folder on the filesystem, and returns it as a File object
	 */
	public File getSapelliFolder()
	{
		if(!isStorageMounted() || !FileHelpers.isReadableWritableDirectory(getStorageDirectory()))
			throw new IllegalStateException("SD card or (emulated) external storage is not accessible");
		if(!sapelliFolder.exists())
		{
			if(!sapelliFolder.mkdirs())
				throw new IllegalStateException("Cannot create Sapelli folder");
		}
		return sapelliFolder;
	}

	public String getDownloadFolderPath()
	{
		return getSapelliFolder().getAbsolutePath() + File.separator + DOWNLOAD_FOLDER;
	}
	
	public String getTempFolderPath()
	{
		return getSapelliFolder().getAbsolutePath() + File.separator + TEMP_FOLDER;
	}
	
	public String getProjectFolderPath()
	{
		return getSapelliFolder().getAbsolutePath() + File.separator + PROJECT_FOLDER;
	}
	
	public String getDumpFolderPath()
	{
		return getSapelliFolder().getAbsolutePath() + File.separator + DUMP_FOLDER;
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
			// Open the db:
			db = DB4OConnector.open(getDatabasePath());
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
				return null; //failed to open db
		}
		daoClients.add(client); //add to set of clients currently using the db
		return new DataAccess(db);
	}
	
	/**
	 * Called by a DataAccessClient to signal it will no longer use its DataAccess object 
	 * 
	 * @param client
	 */
	public void discardDataAccess(DataAccessClient client)
	{
		daoClients.remove(client); //remove client
		if(daoClients.isEmpty() && db != null) //close the db if it is no longer in use
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
		return getFilesDir().getAbsolutePath() + File.separator + (BuildInfo.DEMO_BUILD ? DEMO_PREFIX : "") + DATABASE_NAME;
	}
	
	public void backupDatabase(String filePath)
	{
		db.commit();
		db.ext().backup(filePath);
	}

}
