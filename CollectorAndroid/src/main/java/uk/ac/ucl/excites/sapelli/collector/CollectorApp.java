/**
 * Sapelli data collection platform: http://sapelli.org
 * <p>
 * Copyright 2012-2016 University College London - ExCiteS group
 * <p>
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * <p>
 * http://www.apache.org/licenses/LICENSE-2.0
 * <p>
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.collector;

import android.app.Application;
import android.content.Context;
import android.content.res.Configuration;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.os.Environment;
import android.support.v4.os.EnvironmentCompat;
import android.util.Log;

import com.crashlytics.android.Crashlytics;
import com.crashlytics.android.core.CrashlyticsCore;
import com.facebook.stetho.InspectorModulesProvider;
import com.facebook.stetho.Stetho;
import com.facebook.stetho.inspector.database.DatabaseFilesProvider;
import com.facebook.stetho.inspector.database.DefaultDatabaseConnectionProvider;
import com.facebook.stetho.inspector.database.SqliteDatabaseDriver;
import com.facebook.stetho.inspector.protocol.ChromeDevtoolsDomain;

import org.apache.commons.io.FileUtils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.nio.channels.FileChannel;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import gr.michalisvitos.timberutils.CrashlyticsTree;
import gr.michalisvitos.timberutils.DebugTree;
import io.fabric.sdk.android.Fabric;
import timber.log.Timber;
import uk.ac.ucl.excites.sapelli.collector.db.CollectorPreferences;
import uk.ac.ucl.excites.sapelli.collector.db.CollectorSQLRecordStoreUpgrader;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectRecordStore;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.io.AndroidFileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageRemovedException;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageUnavailableException;
import uk.ac.ucl.excites.sapelli.collector.util.CrashReporter;
import uk.ac.ucl.excites.sapelli.collector.util.ProjectRunHelpers;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreSetter;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.io.FileStorageException;
import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;
import uk.ac.ucl.excites.sapelli.shared.util.android.Debug;
import uk.ac.ucl.excites.sapelli.shared.util.android.DeviceControl;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStoreUpgrader;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.android.AndroidSQLiteRecordStore;

/**
 * Application App to keep the db4o object throughout the life-cycle of the Collector
 *
 * @author Michalis Vitos, mstevens
 */
public class CollectorApp extends Application
{
	static private final String DEMO_PREFIX = "Demo_";
	static public final String DATABASE_BASENAME = "Sapelli";
	static public final String CRASHLYTICS_DEVICE_ID_CRC32 = "SAPELLI_DEVICE_ID_CRC32";
	static public final String CRASHLYTICS_DEVICE_ID_MD5 = "SAPELLI_DEVICE_ID_MD5";

	/**
	 * Used as a System property as well as on Crashlytics.
	 */
	static public final String PROPERTY_LAST_PROJECT = "SAPELLI_LAST_RUNNING_PROJECT";

	// DYNAMICS-----------------------------------------------------------
	private BuildInfo buildInfo;
	public final AndroidCollectorClient collectorClient = new AndroidCollectorClient();
	private CollectorPreferences preferences;


	// Files storage:
	private FileStorageProvider fileStorageProvider;
	private FileStorageException fileStorageException = null;


	public BuildInfo getBuildInfo()
	{
		return buildInfo;
	}

	public void setBuildInfo(BuildInfo buildInfo) {
		this.buildInfo = buildInfo;
	}

	public void setPreferences(CollectorPreferences preferences) {
		this.preferences = preferences;
	}

	public CollectorPreferences getPreferences(){
		return preferences;
	}
	public void setFileStorageProvider(FileStorageProvider fileStorageProvider) {
		this.fileStorageProvider = fileStorageProvider;
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

	@Override
	public void onLowMemory()
	{
		super.onLowMemory();
		Debug.d("onLowMemory() called!");
	}

	/**
	 * Returns a FileStorageProvider when file storage is available or throws an FileStorageUnavailableException or an FileStorageRemovedException if it is not
	 *
	 * @return a PathProvider object
	 * @throws FileStorageException
	 */
	public FileStorageProvider getFileStorageProvider() throws FileStorageException
	{
		if(fileStorageProvider != null)
			return fileStorageProvider;
		if(fileStorageException != null)
			throw fileStorageException;
		else //if(fileStorageProvider == null && fileStorageException == null
			throw new FileStorageUnavailableException("FileStorageProvider has not been initialised yet, please call initialiseFileStorage() first."); // this shouldn't happen
	}

	/**
	 * @return handles for all Stores that need to be backed-up
	 */
	public StoreHandle<?>[] getStoreHandlesForBackup()
	{
		return new StoreHandle[]{collectorClient.recordStoreHandle,
				collectorClient.transmissionStoreHandle,
				collectorClient.projectStoreHandle};
	}

	/**
	 * Returns a prefix to be used on storage identifiers (DB4O filenames, SharedPref's names, etc.) when in demo mode
	 * (if not in demo mode the prefix is empty).
	 * The goal is to separate demo-mode storage from non-demo-mode installations and previous demo installations.
	 *
	 * @return
	 */
	public String getDemoPrefix()
	{
		return (buildInfo.isDemoBuild() ? DEMO_PREFIX + FileHelpers.makeValidFileName(TimeUtils.getTimestampForFileName(buildInfo.getTimeStamp())) : "");
	}

	/**
	 * @author mstevens
	 */
	public final class AndroidCollectorClient extends CollectorClient implements SQLRecordStoreUpgrader.UpgradeCallback
	{

		private int oldDatabaseVersion = CURRENT_COLLECTOR_RECORDSTORE_VERSION;
		private List<String> upgradeWarnings = Collections.<String>emptyList();

		@Override
		public FileStorageProvider getFileStorageProvider()
		{
			return fileStorageProvider;
		}

		@Override
		protected void createAndSetRecordStore(StoreHandle.StoreSetter<RecordStore> setter) throws DBException
		{
			@SuppressWarnings("resource")
			RecordStore recordStore = new AndroidSQLiteRecordStore(this, CollectorApp.this, getFileStorageProvider().getDBFolder(true), getDemoPrefix() /*will be "" if not in demo mode*/ + DATABASE_BASENAME, CURRENT_COLLECTOR_RECORDSTORE_VERSION, new CollectorSQLRecordStoreUpgrader(this, this, getFileStorageProvider()));
			//RecordStore recordStore = new DB4ORecordStore(this, getFileStorageProvider().getDBFolder(true), getDemoPrefix() /*will be "" if not in demo mode*/ + DATABASE_BASENAME);
			setter.setAndInitialise(recordStore);

			// Enable logging if in debug mode (will display SQL statements being executed):
			if(BuildConfig.DEBUG)
				recordStore.setLoggingEnabled(true);
		}

		@Override
		protected void createAndSetProjectStore(StoreHandle.StoreSetter<ProjectStore> setter) throws DBException
		{
			setter.setAndInitialise(new ProjectRecordStore(this, getFileStorageProvider()));
			//setter.setAndInitialise(new PrefProjectStore(CollectorApp.this, getFileStorageProvider(), getDemoPrefix()));
			//setter.setAndInitialise(new DB4OProjectStore(getFileStorageProvider().getDBFolder(true), getDemoPrefix() /*will be "" if not in demo mode*/ + "ProjectStore"));
		}

		@Override
		public void upgradePerformed(int fromVersion, int toVersion, List<String> warnings)
		{
			oldDatabaseVersion = fromVersion;
			upgradeWarnings = warnings;
		}

		public boolean hasDatabaseBeenUpgraded()
		{
			return oldDatabaseVersion != CURRENT_COLLECTOR_RECORDSTORE_VERSION;
		}

		/**
		 * @return the oldDatabaseVersion
		 */
		public final int getOldDatabaseVersion()
		{
			return oldDatabaseVersion;
		}

		/**
		 * @return the upgradeWarnings
		 */
		public final List<String> getUpgradeWarnings()
		{
			return upgradeWarnings;
		}

		public final void forgetAboutUpgrade()
		{
			oldDatabaseVersion = CURRENT_COLLECTOR_RECORDSTORE_VERSION;
			upgradeWarnings = Collections.<String>emptyList();
		}

		@Override
		public void logError(String msg, Throwable throwable)
		{
			if(throwable != null)
				Log.e(getClass().getSimpleName(), msg, throwable);
			else
				Log.e(getClass().getSimpleName(), msg);
		}

		@Override
		public void logWarning(String msg)
		{
			Log.w(getClass().getSimpleName(), msg);
		}

		@Override
		public void logInfo(String msg)
		{
			Log.i(getClass().getSimpleName(), msg);
		}

	}
}
