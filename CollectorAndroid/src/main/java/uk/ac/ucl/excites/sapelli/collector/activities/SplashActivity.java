package uk.ac.ucl.excites.sapelli.collector.activities;

import android.Manifest;
import android.content.Context;
import android.content.Intent;
import android.content.res.Configuration;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.os.Bundle;
import android.os.Environment;
import android.os.Handler;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.os.EnvironmentCompat;

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
import java.util.List;

import gr.michalisvitos.timberutils.CrashlyticsTree;
import gr.michalisvitos.timberutils.DebugTree;
import io.fabric.sdk.android.Fabric;
import pub.devrel.easypermissions.AfterPermissionGranted;
import pub.devrel.easypermissions.EasyPermissions;
import timber.log.Timber;
import uk.ac.ucl.excites.sapelli.collector.BuildConfig;
import uk.ac.ucl.excites.sapelli.collector.BuildInfo;
import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.db.CollectorPreferences;
import uk.ac.ucl.excites.sapelli.collector.io.AndroidFileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageRemovedException;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageUnavailableException;
import uk.ac.ucl.excites.sapelli.collector.util.CrashReporter;
import uk.ac.ucl.excites.sapelli.collector.util.ProjectRunHelpers;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.io.FileStorageException;
import uk.ac.ucl.excites.sapelli.shared.util.android.Debug;
import uk.ac.ucl.excites.sapelli.shared.util.android.DeviceControl;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore;

import static uk.ac.ucl.excites.sapelli.collector.CollectorApp.DATABASE_BASENAME;

public class SplashActivity extends AppCompatActivity implements EasyPermissions.PermissionCallbacks {

    // STATICS------------------------------------------------------------
    static protected final String TAG = "CollectorApp";

    static private final String CRASHLYTICS_VERSION_INFO = "VERSION_INFO";
    static private final String CRASHLYTICS_BUILD_INFO = "BUILD_INFO";
    static private final int PERMISSIONS_REQUEST = 123;
    String[] perms = {
            Manifest.permission.CAMERA,
            Manifest.permission.ACCESS_FINE_LOCATION,
            Manifest.permission.RECORD_AUDIO,
            Manifest.permission.WRITE_EXTERNAL_STORAGE,
            Manifest.permission.READ_PHONE_STATE,
            Manifest.permission.READ_EXTERNAL_STORAGE
    };
    private CollectorApp app;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_splash);

        if (!isAllPermissionsGranted()) {
            requestAllPermissions();
            return;
        }

        initializations();
    }

    private void requestAllPermissions() {
        EasyPermissions.requestPermissions(this, null,
                PERMISSIONS_REQUEST, perms);
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        EasyPermissions.onRequestPermissionsResult(requestCode, permissions, grantResults, this);
    }

    private boolean isAllPermissionsGranted() {
        return EasyPermissions.hasPermissions(this, perms);
    }

    @AfterPermissionGranted(PERMISSIONS_REQUEST)
    private void initializations() {
        // Build info:
        getCollectorApp().setBuildInfo(BuildInfo.GetInstance(getApplicationContext()));

        Debug.d("CollectorApp started.\nBuild info:\n" + getCollectorApp().getBuildInfo().getAllInfo());

        // Start Fabric
        setFabric();

        // Set Timber for logging
        setTimber();

        // Set Stetho for debugging
        setStetho();

        // Get collector preferences:
        getCollectorApp().setPreferences(new CollectorPreferences(getApplicationContext()));

        // Initialise file storage:
        try {
            getCollectorApp().setFileStorageProvider(initialiseFileStorage()); // throws FileStorageException
        } catch (FileStorageException fse) {
//            getCollectorApp().fileStorageException = fse; // postpone throwing until getFileStorageProvider() is called!
        }

        // Set up a CrashReporter (will use dumps folder):
        if (getCollectorApp().getFileStorageProvider() != null)
            Thread.setDefaultUncaughtExceptionHandler(new CrashReporter(getCollectorApp().getFileStorageProvider(), getResources().getString(R.string.app_name)));

        // Create shortcut to Sapelli Collector on Home Screen:
        if (getCollectorApp().getPreferences().isFirstInstallation()) {
            // Create shortcut
            ProjectRunHelpers.createCollectorShortcut(getApplicationContext());
            // Set first installation to false
            getCollectorApp().getPreferences().setFirstInstallation(false);
        }

        new Handler().postDelayed(new Runnable() {
            @Override
            public void run() {
                startActivity(new Intent(SplashActivity.this, ProjectManagerActivity.class));
                finish();
            }
        }, 1000);
    }

    /**
     * Set up Fabric
     */
    private void setFabric() {
        // Set up Crashlytics, disabled for debug builds
        final CrashlyticsCore crashlyticsCore = new CrashlyticsCore.Builder().disabled(BuildConfig.DEBUG).build();
        final Crashlytics crashlyticsKit = new Crashlytics.Builder().core(crashlyticsCore).build();
        Fabric.with(this, crashlyticsKit);

        Crashlytics.setString(CRASHLYTICS_VERSION_INFO, getCollectorApp().getBuildInfo().getNameAndVersion() + " [" + getCollectorApp().getBuildInfo().getExtraVersionInfo() + "]");
        Crashlytics.setString(CRASHLYTICS_BUILD_INFO, getCollectorApp().getBuildInfo().getBuildInfo());
    }

    /**
     * Set up Timber for logging
     */
    private void setTimber() {
        // Enable Timber
        if (BuildConfig.DEBUG)
            Timber.plant(new DebugTree());
        else
            Timber.plant(new CrashlyticsTree());
    }

    /**
     * Set up Stetho for debugging
     */
    private void setStetho() {
        // Enable Stetho in Debug versions
        if (!BuildConfig.DEBUG)
            return;

        Timber.d("Enable Stetho");

        Stetho.initialize(Stetho.newInitializerBuilder(this)
                .enableWebKitInspector(new InspectorModulesProvider() {
                    @Override
                    public Iterable<ChromeDevtoolsDomain> get() {
                        return new Stetho.DefaultInspectorModulesBuilder(SplashActivity.this)
                                .provideDatabaseDriver(createCustomDatabaseDriver(SplashActivity.this))
                                .finish();
                    }
                }).build());
    }

    private SqliteDatabaseDriver createCustomDatabaseDriver(Context context) {
        return new SqliteDatabaseDriver(context, new DatabaseFilesProvider() {
            @Override
            public List<File> getDatabaseFiles() {
                List<File> dbs = new ArrayList<>();
                final String dbPath = SQLiteRecordStore.GetDBFileName(getCollectorApp().getFileStorageProvider().getDBFolder(false).getAbsolutePath() + File.separator + getCollectorApp().DATABASE_BASENAME);
                Timber.d("Try to connect to db at: %s", dbPath);
                dbs.add(new File(dbPath));
                return dbs;
            }
        }, new DefaultDatabaseConnectionProvider());
    }

    /**
     * @return
     * @throws FileStorageException
     */
    private FileStorageProvider initialiseFileStorage() throws FileStorageException {
        File sapelliFolder = null;

        // Try to get Sapelli folder path from preferences:
        try {
            sapelliFolder = new File(getCollectorApp().getPreferences().getSapelliFolderPath());
        } catch (NullPointerException npe) {
        }

        // Did we get the folder path from preferences? ...
        if (sapelliFolder == null) {    // No: first installation or reset

            // Find appropriate files dir (using application-specific folder, which is removed upon app uninstall!):
            File[] paths = DeviceControl.getExternalFilesDirs(this, null);
            if (paths != null && paths.length != 0) {
                // We count backwards because we prefer secondary external storage (which is likely to be on an SD card rather unremovable memory)
                for (int p = paths.length - 1; p >= 0; p--)
                    if (isMountedReadableWritableDir(paths[p])) {
                        sapelliFolder = paths[p];
                        break;
                    }
            }

            // Do we have a path?
            if (sapelliFolder != null)
                // Yes: store it in the preferences:
                getCollectorApp().getPreferences().setSapelliFolder(sapelliFolder.getAbsolutePath());
            else
                // No :-(
                throw new FileStorageUnavailableException();
        } else {    // Yes, we got path from preferences, check if it is available ...
            if (!isMountedReadableWritableDir(sapelliFolder)) // (will also attempt to create the directory if it doesn't exist)
                // No :-(
                throw new FileStorageRemovedException(sapelliFolder.getAbsolutePath());
        }

        // If we get here this means we have a non-null sapelliFolder object representing an accessible path...

        // Try to get the Android Downloads folder...
        File downloadsFolder = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS);
        if (!isMountedReadableWritableDir(downloadsFolder)) // check if we can access it (will also attempt to create the directory if it doesn't exist)
            // No :-(
            throw new FileStorageException("Cannot access downloads folder: " + downloadsFolder.getAbsolutePath());

        // Create a test database to get the DB folder
        TestDatabaseHelper testDatabaseHelper = new TestDatabaseHelper(this);

        // Try to get the Databases folder...
        File databaseFolder = testDatabaseHelper.getDatabaseFolder();
        if (!isMountedReadableWritableDir(databaseFolder)) // check if we can access it (will also attempt to create the directory if it doesn't exist)
            // No :-(
            throw new FileStorageException("Cannot access database folder: " + databaseFolder.getAbsolutePath());

        final AndroidFileStorageProvider androidFileStorageProvider = new AndroidFileStorageProvider(sapelliFolder, databaseFolder, downloadsFolder);

        moveDB(androidFileStorageProvider);

        return androidFileStorageProvider; // Android specific subclass of FileStorageProvider, which generates .nomedia files
    }

    /**
     * The location for the Sapelli database has changed on Sapelli v2.0.0 beta 27, therefore move
     * the DB from the old location to the new one.
     *
     * @param androidFileStorageProvider
     */
    private void moveDB(AndroidFileStorageProvider androidFileStorageProvider) {

        final File oldDBFolder = androidFileStorageProvider.getOldDBFolder(false);
        final File nedDBFolder = androidFileStorageProvider.getDBFolder(false);
        Timber.d("Old DB path: %s", oldDBFolder);
        Timber.d("New DB path: %s", nedDBFolder);

        try {
            File oldDB = new File(SQLiteRecordStore.GetDBFileName(oldDBFolder.getAbsolutePath() + File.separator + DATABASE_BASENAME));
            File newDB = new File(nedDBFolder + File.separator + oldDB.getName());

            if (!newDB.exists())
                newDB.createNewFile();

            if (oldDB.exists() && newDB.exists()) {
                Timber.d("Move Old DB: %s to %s", oldDB, newDB);

                FileChannel src = new FileInputStream(oldDB).getChannel();
                FileChannel dst = new FileOutputStream(newDB).getChannel();
                dst.transferFrom(src, 0, src.size());
                src.close();
                dst.close();

                // Delete the old DB
                FileUtils.deleteQuietly(oldDB);

                // Delete all other files in the old DB e.g. the journal etc.
                for (File file : oldDBFolder.listFiles())
                    FileUtils.deleteQuietly(file);

                // Finally delete the old directory
                FileUtils.deleteQuietly(oldDBFolder);
            }
        } catch (Exception e) {
            Timber.e(e);
        }
    }

    /**
     * Check if a directory is on a mounted storage and writable/readable
     *
     * @param dir
     * @return
     * @throws FileStorageException
     */
    private boolean isMountedReadableWritableDir(File dir) throws FileStorageException {
        try {
            return    // Null check:
                    (dir != null)
                            // Try to create the directory if it is not there
                            && FileHelpers.createDirectory(dir)
                            /* Check storage state, accepting both MEDIA_MOUNTED and MEDIA_UNKNOWN.
                             * 	The MEDIA_UNKNOWN state occurs when a path isn't backed by known storage media; e.g. the SD Card on
                             * the Samsung Xcover 2 (the detection of which we have to force in DeviceControl#getExternalFilesDirs()). */
                            && (Environment.MEDIA_MOUNTED.equals(EnvironmentCompat.getStorageState(dir)) || EnvironmentCompat.MEDIA_UNKNOWN.equals(EnvironmentCompat.getStorageState(dir)))
                            // Check whether we have read & write access to the directory:
                            && FileHelpers.isReadableWritableDirectory(dir);
        } catch (Exception e) {
            throw new FileStorageException("Unable to create or determine status of directory: " + (dir != null ? dir.getAbsolutePath() : "null"), e);
        }
    }

    @Override
    public void onConfigurationChanged(Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        // Debug.d(newConfig.toString());
    }

    public CollectorApp getCollectorApp() {
        if (app == null)
            app = (CollectorApp) getApplication();
        return app;
    }

    public CollectorApp.AndroidCollectorClient getCollectorClient() {
        return getCollectorApp().collectorClient;
    }

    @Override
    public void onPermissionsGranted(int requestCode, @NonNull List<String> perms) {
    }

    @Override
    public void onPermissionsDenied(int requestCode, @NonNull List<String> perms) {
        recreate();
    }

    public static enum StorageStatus {
        UNKNOWN, STORAGE_OK, STORAGE_UNAVAILABLE, STORAGE_REMOVED
    }

    /**
     * Create a "Test.db" in the default location of Android. Use this to get the directory where
     * Android stores by default the SQLite database
     */
    public class TestDatabaseHelper extends SQLiteOpenHelper {
        private static final String DATABASE_NAME = "Test.db";
        private static final int DATABASE_VERSION = 1;
        private Context context;

        public TestDatabaseHelper(Context context) {
            super(context, DATABASE_NAME, null, DATABASE_VERSION);
            this.context = context;
        }

        @Override
        public void onCreate(SQLiteDatabase sqLiteDatabase) {
            // Do nothing
        }

        @Override
        public void onUpgrade(SQLiteDatabase sqLiteDatabase, int i, int i1) {
            // Do nothing
        }

        public File getDatabaseFolder() {
            return context.getDatabasePath(DATABASE_NAME).getParentFile();
        }
    }
}
