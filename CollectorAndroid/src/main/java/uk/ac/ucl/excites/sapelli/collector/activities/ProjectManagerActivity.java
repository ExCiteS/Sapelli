/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.collector.activities;

import android.app.Activity;
import android.content.ActivityNotFoundException;
import android.content.Intent;
import android.graphics.Typeface;
import android.net.Uri;
import android.os.Bundle;
import androidx.viewpager.widget.ViewPager;
import androidx.drawerlayout.widget.DrawerLayout;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.ActionBarDrawerToggle;
import androidx.appcompat.widget.Toolbar;
import android.util.Log;
import android.util.Patterns;
import android.util.TypedValue;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.WindowManager;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

import com.astuetz.PagerSlidingTabStrip;
import com.crashlytics.android.Crashlytics;
import com.ipaulpro.afilechooser.utils.FileUtils;

import java.io.File;
import java.util.Collections;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.CollectorApp.AndroidCollectorClient;
import uk.ac.ucl.excites.sapelli.collector.CollectorClient;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.fragments.ExportFragment;
import uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerTabFragment;
import uk.ac.ucl.excites.sapelli.collector.fragments.ProjectManagerTabFragmentPagerAdapter;
import uk.ac.ucl.excites.sapelli.collector.fragments.dialogs.AboutFragment;
import uk.ac.ucl.excites.sapelli.collector.fragments.dialogs.EnterURLFragment;
import uk.ac.ucl.excites.sapelli.collector.fragments.dialogs.ManageReceiversFragment;
import uk.ac.ucl.excites.sapelli.collector.fragments.tabs.MainTabFragment;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider;
import uk.ac.ucl.excites.sapelli.collector.load.AndroidProjectLoaderStorer;
import uk.ac.ucl.excites.sapelli.collector.load.ProjectLoader;
import uk.ac.ucl.excites.sapelli.collector.load.ProjectLoaderStorer;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.model.ProjectDescriptor;
import uk.ac.ucl.excites.sapelli.collector.tasks.Backup;
import uk.ac.ucl.excites.sapelli.collector.tasks.ProjectTasks;
import uk.ac.ucl.excites.sapelli.collector.tasks.RecordsTasks;
import uk.ac.ucl.excites.sapelli.collector.util.AsyncDownloader;
import uk.ac.ucl.excites.sapelli.collector.util.AsyncTaskWithWaitingDialog;
import uk.ac.ucl.excites.sapelli.collector.util.DeviceID;
import uk.ac.ucl.excites.sapelli.collector.util.ProjectRunHelpers;
import uk.ac.ucl.excites.sapelli.collector.util.qrcode.IntentIntegrator;
import uk.ac.ucl.excites.sapelli.collector.util.qrcode.IntentResult;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreUser;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.io.FileStorageException;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.shared.util.TransactionalStringBuilder;
import uk.ac.ucl.excites.sapelli.shared.util.android.MenuHelpers;
import uk.ac.ucl.excites.sapelli.storage.eximport.ExportResult;
import uk.ac.ucl.excites.sapelli.storage.eximport.csv.CSVRecordsExporter;
import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsExporter;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;

/**
 * @author Julia, Michalis Vitos, mstevens, benelliott
 * 
 */
public class ProjectManagerActivity extends BaseActivity implements StoreUser, DeviceID.InitialisationCallback, ProjectLoaderStorer.FileSourceCallback, AsyncDownloader.Callback, ListView.OnItemClickListener, RecordsTasks.ImportCallback
{

	// STATICS--------------------------------------------------------
	static private final String TAG = "ProjectManagerActivity";

	static protected final String XML_FILE_EXTENSION = "xml";

	static protected final String DEMO_PROJECT = "demo.excites";

	public static final int RETURN_BROWSE_FOR_PROJECT_LOAD = 1;
	public static final int RETURN_BROWSE_FOR_RECORD_IMPORT = 2;
	
	private static final int PAGER_MARGIN_DIP = 4;
	
	static public final Typeface FONT_SANS_SERIF_CONDENSED = Typeface.create("sans-serif-condensed", Typeface.NORMAL);

	// DYNAMICS-------------------------------------------------------
	private DeviceID deviceID;
	private ProjectStore projectStore;
	private Project currentProject;
	
	private ArrayAdapter<ProjectDescriptor> projectListAdaptor;

	// UI
	private TextView addProjects;
	private ProjectManagerTabFragmentPagerAdapter pagerAdapter;
	private ViewPager pager;
	private PagerSlidingTabStrip tabs;
	private TextView lblAvailableProjects;
	private ListView projectList;
	private Toolbar mainToolbar;
	private Toolbar drawerToolbar;
	private ActionBarDrawerToggle drawerToggle;
	private DrawerLayout drawerLayout;
	
	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		
		if(getCollectorApp().getBuildInfo().isDemoBuild())
			return;
		//else ...
		// Only if not in demo mode:
		
		// Hide soft keyboard on create
		getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_HIDDEN);
		
		// Set-up UI...
		setContentView(R.layout.activity_projectmanager);
		lblAvailableProjects = (TextView) findViewById(R.id.lblAvailableProjects);
		projectList = (ListView) findViewById(R.id.projectList);
		drawerLayout = (DrawerLayout) findViewById(R.id.drawer_layout);
		mainToolbar = (Toolbar) findViewById(R.id.projMngrToolbar);
		drawerToolbar = (Toolbar) findViewById(R.id.drawerToolbar);
		addProjects = (TextView) findViewById(R.id.addProjects);
		//	Pager & tabs:
		pager = (ViewPager) findViewById(R.id.pager);
		pager.setPageMargin((int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, PAGER_MARGIN_DIP, getResources().getDisplayMetrics()));
		pagerAdapter = new ProjectManagerTabFragmentPagerAdapter(this, getSupportFragmentManager());
		pager.setAdapter(pagerAdapter);
		tabs = (PagerSlidingTabStrip) findViewById(R.id.tabs);
		tabs.setViewPager(pager);
		
		// We use a Toolbar as the ActionBar:
		mainToolbar.setSubtitleTextAppearance(this, R.style.TextAppearance_Sap_ActionBarSubTitleText); // somehow I could specific this at the XML level (see styles.xml)
		setSupportActionBar(mainToolbar);
		
		// Enable ActionBar icon to behave as action to toggle drawer
		ActionBar actionBar = getSupportActionBar();
		actionBar.setHomeButtonEnabled(true);
		actionBar.setDisplayShowHomeEnabled(true);

		// ActionBarDrawerToggle ties together the the proper interactions
		// between the navigation drawer and the action bar app icon.
		drawerToggle = new ActionBarDrawerToggle(this, drawerLayout, mainToolbar, 0, 0)
		{
			@Override
			public void onDrawerClosed(View drawerView)
			{
				super.onDrawerClosed(drawerView);
				supportInvalidateOptionsMenu(); // calls onPrepareOptionsMenu()
			}

			@Override
			public void onDrawerOpened(View drawerView)
			{
				super.onDrawerOpened(drawerView);
				supportInvalidateOptionsMenu(); // calls onPrepareOptionsMenu()
			}
		};

		// Defer code dependent on restoration of previous instance state.
		// NB: required for the drawer indicator to show up!
		drawerLayout.post(new Runnable()
		{
			@Override
			public void run()
			{
				drawerToggle.syncState();
			}
		});
		
		// Set the drawer toggle as the DrawerListener
		drawerLayout.setDrawerListener(drawerToggle);
		
		// Set the list's click listener
		projectList.setOnItemClickListener(this);
		
		// Set the drawerToolbar title & menu:
		drawerToolbar.setTitle(getString(R.string.add_new_project) + ":");
		drawerToolbar.inflateMenu(R.menu.projectload);
		// 	Force displaying of icons in overflow menu:
		MenuHelpers.forceMenuIcons(drawerToolbar.getMenu());
	}
		
	@Override
	public boolean onCreateOptionsMenu(Menu menu)
	{
		// Inflate main actionbar menu:
		getMenuInflater().inflate(R.menu.projectmanager, menu);
		
		// Inflate & set projectload submenu:
		getMenuInflater().inflate(R.menu.projectload, menu.findItem(R.id.action_load).getSubMenu());
		
		// Dis/enable remove action:
		menu.findItem(R.id.action_remove).setVisible(currentProject != null);

		// Force displaying of icons in overflow menu:
		MenuHelpers.forceMenuIcons(menu);
		
		return true;
	}
	
	@Override
	protected void onResume()
	{
		super.onResume();
		
		// Initialise DeviceID:
		DeviceID.Initialise(this, this); // will post a callback upon completion (success/failure)
		
		// Get ProjectStore instance:
		try
		{
			projectStore = getCollectorClient().projectStoreHandle.getStore(this);
		}
		catch(Exception e)
		{
			Log.e(TAG, getString(R.string.storageAccessFail), e);
			showErrorDialog(getString(R.string.storageAccessFail, ExceptionHelpers.getMessageAndCause(e)), true);
			return;
		}
		
		// Check & report on database upgrade:
		AndroidCollectorClient client = getCollectorClient(); 
		if(client.hasDatabaseBeenUpgraded())
		{
			List<String> warnings = client.getUpgradeWarnings();
			showWarningDialog(getString(R.string.dbUpgrade, client.getOldDatabaseVersion(), CollectorClient.CURRENT_COLLECTOR_RECORDSTORE_VERSION) + (!warnings.isEmpty() ? "\n\n" + listWarnings(R.string.dbUpgradeWarningsTitle, warnings) : ""));
			client.forgetAboutUpgrade(); // otherwise the dialog box appears for every subsequent onResume()
		}
		
		// And finally...
		/*if(app.getBuildInfo().isDemoBuild())
			demoMode();
		else*/
		updateProjectList(false);
		
		// stop tracing
		//Debug.stopMethodTracing();
	}
	
	/*private void demoMode()
	{
		try
		{
			List<Project> projects = projectStore.retrieveProjects();
			Project p = null;
			if(projects.isEmpty())
			{	// Use /mnt/sdcard/Sapelli/ as the basePath:
				p = new ProjectLoader(fileStorageProvider).load(this.getAssets().open(DEMO_PROJECT, AssetManager.ACCESS_RANDOM));
				projectStore.add(p);
			}
			else
				p = projects.get(0); // Assumption: there is only one stored project in demo mode
			// Run the project
			startActivity(ProjectRunHelpers.getProjectRunIntent(this, p));
		}
		catch(Exception e)
		{
			Log.e(TAG, "Error loading/storing/launching demo project", e);
			showErrorDialog(R.string.demoLoadFail, true);
		}
	}*/

	@Override
	public void initialisationSuccess(DeviceID deviceID)
	{
		this.deviceID = deviceID;

		// Fabric info
		Crashlytics.setLong(CollectorApp.CRASHLYTICS_DEVICE_ID_CRC32, deviceID.getIDAsCRC32Hash());
		Crashlytics.setString(CollectorApp.CRASHLYTICS_DEVICE_ID_MD5, deviceID.getIDAsMD5Hash().toString());
	}

	@Override
	public void initialisationFailure(DeviceID deviceID)
	{
		deviceID.printInfo();
		showErrorDialog(R.string.noDeviceID, true);
	}

	/**
	 * @return the deviceID
	 */
	public DeviceID getDeviceID()
	{
		return deviceID;
	}

	private void updateProjectList(boolean force)
	{
		if(projectListAdaptor == null || force)
			new RetrieveProjectDescriptorsTask().execute();
	}
	
	private class RetrieveProjectDescriptorsTask extends AsyncTaskWithWaitingDialog<ProjectManagerActivity, Void, List<ProjectDescriptor>>
	{
		
		public RetrieveProjectDescriptorsTask()
		{
			super(ProjectManagerActivity.this, getString(R.string.load_projects));
		}

		@Override
		protected List<ProjectDescriptor> runInBackground(Void... params)
		{
			return projectStore.retrieveProjectsOrDescriptors();
		}

		@Override
		protected void onPostExecute(List<ProjectDescriptor> result)
		{
			super.onPostExecute(result); // dismiss dialog

			// Sort alphabetically:
			Collections.sort(result);
			
			// Update drawer list:
			projectListAdaptor = new ArrayAdapter<ProjectDescriptor>(ProjectManagerActivity.this, R.layout.project_item, result);
			projectList.setAdapter(projectListAdaptor);
			
			// Set label above list:
			lblAvailableProjects.setText(result.isEmpty() ? R.string.no_projects : R.string.switch_project);
			
			// Determine which project to switch to:
			ProjectDescriptor switchTo = null; // if this stays null we will go into "no-project mode"
			if(!result.isEmpty())
			{
				// In case the currentProject and the previously active one are no longer
				//	available (see below) we switch to the first project in the list:
				switchTo = result.get(0);
				
				// If there is a currently active project...
				if(currentProject != null)
				{	// ... look for it in the result list:
					for(int i = 0; i < result.size(); i++)
						if(result.get(i).equalDescription(currentProject))
						{	// (Re)select current project:
							switchTo = result.get(i);
							break;
						}
					// if we get here the currentProject is no longer available
				}
				// If there is a previously active project...
				else if(getPreferences().getActiveProjectSignature() != null)
				{
					// ... look for it in the result list:
					String prevActiveProjectSign = getPreferences().getActiveProjectSignature();
					for(int i = 0; i < result.size(); i++)
						if(result.get(i).getSignatureString().equals(prevActiveProjectSign))
						{	// (Re)select previously active project:
							switchTo = result.get(i);
							break;
						}
					// if we get here the previously active project is no longer available
				}
			}
			
			// Switch to the right project:
			switchToProject(switchTo);
		}

	}
	
	/**
	 * Called when user selected a Project(Descriptor) from the Drawer
	 * 
	 * @param parent
	 * @param view
	 * @param position
	 * @param id
	 */
	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position, long id)
	{
		// Switch project:
		switchToProject(projectListAdaptor.getItem(position));

		// Close drawer:
		closeDrawer(null);
	}
	
	private void switchToProject(final ProjectDescriptor projDescr)
	{
		// Select in list:
		if(projDescr != null && projectListAdaptor.getPosition(projDescr) != -1)
			projectList.setItemChecked(projectListAdaptor.getPosition(projDescr), true);
		
		// If we got null or a Project object (instead of a plain ProjectDesciptor):
		if(projDescr == null || projDescr instanceof Project)
			setCurrentProject((Project) projDescr);
		
		// If we got a ProjectDesciptor:
		else if(projDescr.equals(currentProject))
			return; // this is already the current project
		
		else // Parse project:
			new ProjectTasks.ReloadProjectTask(this, projectStore, new ProjectTasks.ReloadProjectCallback()
			{
				@Override
				public void projectReloaded(Project project)
				{
					// Set as current project:
					setCurrentProject(project);
					
					// Refresh drawer list (to use project instead of projectDescriptor instance):
					updateProjectList(true);
				}

				@Override
				public void projectReloadFailure()
				{	// Project could not be loaded (likely the XML file is no longer there), make sure it is fully removed:
					removeProject(projDescr); // will also update the project list
				}
			}).execute(projDescr);
	}
	
	/**
	 * @param project - may be null
	 */
	private void setCurrentProject(Project project)
	{
		if(currentProject == project && currentProject != null)
			return; // this is already the current project
		//else...
		currentProject = project;
		
		// Remember current project:
		getPreferences().setActiveProjectSignature(currentProject);
		
		// Update UI:
		if(currentProject != null)
		{
			getSupportActionBar().setTitle(currentProject.getName());
			getSupportActionBar().setSubtitle(currentProject.getVariantVersionString());
			tabs.setVisibility(View.VISIBLE);
			pager.setVisibility(View.VISIBLE);
			addProjects.setVisibility(View.GONE);
		}
		else
		{
			getSupportActionBar().setTitle(R.string.sapelli);
			getSupportActionBar().setSubtitle(R.string.collector);
			tabs.setVisibility(View.GONE);
			pager.setVisibility(View.GONE);
			addProjects.setVisibility(View.VISIBLE);
		}
		// 	Refresh tabs:
		pagerAdapter.refresh();
		// 	Refresh menus:
		supportInvalidateOptionsMenu();
	}
	
	public Project getCurrentProject(boolean errorIfNull)
	{
		if(currentProject == null)
		{
			if(errorIfNull)
				showErrorDialog(R.string.selectProject, false);
			return null;
		}
		return currentProject;
	}
	
	/**
	 * Browse for project file to load project from.
	 * 
	 * @param menuItem
	 */
	public void browse(MenuItem menuItem)
	{
		// Open file picker to let user chose a project file to load:
		try
		{
			// Use the GET_CONTENT intent from the utility class
			Intent target = FileUtils.createGetContentIntent(null);
			
			// Create the chooser Intent
			Intent intent = Intent.createChooser(target, getString(R.string.chooseSapelliFile));
			
			// Start file picker activity:
			startActivityForResult(intent, RETURN_BROWSE_FOR_PROJECT_LOAD);
		}
		catch(ActivityNotFoundException e){}
		
		// Close drawer:
		closeDrawer(null); // won't do anything if it is not open
	}
	
	public void enterURL(MenuItem menuItem)
	{
		closeDrawer(null);		
		new EnterURLFragment().show(getSupportFragmentManager(), getString(R.string.enter_url));
	}
	
	public void switchToTab(int index)
	{
		pager.setCurrentItem(index);
	}

	@Override
	protected void onDestroy()
	{
		Log.d(TAG, "Destroying...");
		
		// signal that the activity no longer needs the stores (projecStore and its internally use transmissionStore instance):
		getCollectorApp().collectorClient.projectStoreHandle.doneUsing(this);
		
		// super:
		super.onDestroy();
	}
	
	public void closeDrawer(View view)
	{
		drawerLayout.closeDrawers();
	}

	/**
	 * @param view
	 */
	public void openAboutDialog(View view)
	{
		openAboutDialog();
		closeDrawer(null);
	}
	
	/**
	 * @param item
	 * @return
	 */
	public boolean openAboutDialog(MenuItem item)
	{
		openAboutDialog();
		return true;
	}
	
	private void openAboutDialog()
	{
		new AboutFragment().show(getSupportFragmentManager(), getString(R.string.about));
	}
	
	/**
	 * @param item
	 * @return
	 */
	public boolean manageReceivers(MenuItem item)
	{
		ManageReceiversFragment.ShowDialog(this, null);
		return true;
	}

	/**
	 * Export all projects
	 * 
	 * @param item
	 * @return
	 */
	public boolean exportRecords(MenuItem item)
	{
		ExportFragment.ShowExportAllDialog(this, true);
		return true;
	}

	/**
	 * To be called upon completion of a record/data export (and possibly delete) operation
	 * 
	 * @param result
	 * @param exportedDataDeleted
	 */
	public void onDataExportDone(ExportResult result, boolean exportedDataDeleted)
	{
		if(exportedDataDeleted)
			onDataChanged();
	}
	
	/**
	 * To be called upon completion of a record/data delete operation
	 */
	public void onDataChanged()
	{
		// Refresh all tabs:
		refreshAllTabs();
		// Alternative (refresh only main tab):
		//refreshTap(MainTabFragment.class);
	}
	
	public void refreshAllTabs()
	{
		pagerAdapter.refresh();
	}
	
	public void refreshTab(Class<? extends ProjectManagerTabFragment> tabClass)
	{
		try
		{
			pagerAdapter.getTab(tabClass).refresh();
		}
		catch(Exception e)
		{
			Log.e(TAG, "Error upon refreshing tab: " + tabClass != null ? tabClass.getSimpleName() : "null", e);
		}
	}

	public boolean importRecords(MenuItem item)
	{
		// Use the GET_CONTENT intent from the utility class
		Intent target = FileUtils.createGetContentIntent(null);
		// Create the chooser Intent
		Intent intent = Intent.createChooser(target, "Choose an XML or CSV file");
		try
		{
			startActivityForResult(intent, RETURN_BROWSE_FOR_RECORD_IMPORT);
		}
		catch(ActivityNotFoundException e){}

		return true;
	}
	
	/**
	 * Create Sapelli back-up package
	 * 
	 * @param item
	 * @return
	 */
	public boolean backupSapelli(MenuItem item)
	{
		runFileStorageTask(new FileStorageTask()
		{
			@Override
			public void run(FileStorageProvider fsp)
			{
				Backup.Run(ProjectManagerActivity.this, fsp);
			}
		});
		return true;
	}

	public void runProject(View view)
	{
		Project p = getCurrentProject(true);
		if(p != null)
			startActivity(ProjectRunHelpers.getProjectRunIntent(this, p));
	}

	public void loadProject(String path)
	{
		// Check path:
		if(path == null || path.trim().isEmpty())
			return;
		
		final String location = path.trim();
		if(Patterns.WEB_URL.matcher(location).matches())
		{	// Location is a (remote) URL: download Sapelli file:
			runFileStorageTask(new FileStorageTask()
			{
				@Override
				public void run(FileStorageProvider fsp)
				{
					try
					{	// loading & storing of the project will happen upon successful download (via callback)
						AsyncDownloader.Download(ProjectManagerActivity.this, fsp.getSapelliDownloadsFolder() /*throws FileStorageException*/, location, ProjectManagerActivity.this);
					}
					catch(FileStorageException fse)
					{
						showErrorDialog(getString(R.string.storageError, fse.getMessage()));
					}	
				}
			});
		}
		else
			// Load project from local file:
			loadProject(new File(location), null, true);
	}
	
	@Override
	public void downloadSuccess(String downloadUrl, File downloadedFile)
	{
		loadProject(downloadedFile, downloadUrl, false); // don't check extension for downloaded files
	}

	@Override
	public void downloadFailure(String downloadUrl, Exception cause)
	{
		showErrorDialog(
			cause != null ?
				getString(R.string.downloadErrorWithCause, ExceptionHelpers.getMessage(cause)) :
				getString(R.string.downloadError),
			false);
	}
	
	/**
	 * @param localFile
	 * @param sourceURI - may be null
	 * @param checkExtension
	 */
	public void loadProject(final File localFile, final String sourceURI, boolean checkExtension)
	{
		if(!FileHelpers.isReadableFile(localFile))
		{
			showErrorDialog(R.string.invalidFile);
		}
		else if(checkExtension && XML_FILE_EXTENSION.equalsIgnoreCase(FileHelpers.getFileExtension(localFile)))
		{
			showErrorDialog(R.string.noBareXMLProjects);
		}
		else if(checkExtension && !ProjectLoader.HasSapelliFileExtension(localFile))
		{	// Warn about extension:
			showOKCancelDialog(
				R.string.warning,
				getString(R.string.unsupportedExtension, FileHelpers.getFileExtension(localFile), StringUtils.join(ProjectLoader.SAPELLI_FILE_EXTENSIONS, ", ")),
				false,
				new Runnable()
				{
					@Override
					public void run()
					{
						loadProject(localFile, sourceURI, false); // try loading anyway
					}
				},
				false);
		}
		else
		{	// Actually load & store the project:
			runFileStorageTask(new FileStorageTask()
			{
				@Override
				public void run(FileStorageProvider fsp)
				{
					new AndroidProjectLoaderStorer(ProjectManagerActivity.this, fsp, projectStore).loadAndStore(
						localFile,
						sourceURI != null ? sourceURI : Uri.fromFile(localFile).toString(),
						ProjectManagerActivity.this);
				}
			});
		}

	}
	
	@Override
	public void projectLoadStoreSuccess(File sapelliFile, String sourceURI, Project project, List<String> warnings)
	{
		// Deal with downloaded file...
		if(Patterns.WEB_URL.matcher(sourceURI).matches())
			// Change temporary name to proper one:
			sapelliFile.renameTo(new File(sapelliFile.getParentFile().getAbsolutePath() + File.separator + FileHelpers.makeValidFileName(project.toString(false) + ".sapelli")));
		
		// Warnings...
		TransactionalStringBuilder bldr = new TransactionalStringBuilder("\n");
		//	Parser/loader warnings: 
		if(!warnings.isEmpty())
			bldr.append(listWarnings(R.string.projectLoadingWarnings, warnings));
		//	Check file dependencies:
		List<String> missingFiles = project.getMissingFilesRelativePaths(getFileStorageProvider());
		if(!missingFiles.isEmpty())
		{
			bldr.append(getString(R.string.missingFiles) + ":");
			for(String missingFile : missingFiles)
				bldr.append(" - " + missingFile);
		}
		//	Show warnings dialog:
		if(!bldr.isEmpty())
			showWarningDialog(bldr.toString()); // no need to worry about message not fitting the dialog, it will have a scrollbar when necessary 
		
		// Select project:
		setCurrentProject(project);
		
		// Update project list:
		updateProjectList(true);
	}

	@Override
	public void projectLoadStoreFailure(File sapelliFile, String sourceURI, Exception cause)
	{
		// Deal with downloaded file...
		if(Patterns.WEB_URL.matcher(sourceURI).matches())
			org.apache.commons.io.FileUtils.deleteQuietly(sapelliFile);
		// Report problem:
		Log.e(TAG, "Could not load/store Sapelli file", cause);
		showErrorDialog(getString(R.string.sapelliFileLoadFailure, (Patterns.WEB_URL.matcher(sourceURI).matches() ? sapelliFile.getAbsolutePath() : sourceURI), ExceptionHelpers.getMessageAndCause(cause)), false);		
	}
	
	public void scanQR(MenuItem item)
	{
		// Start the Intent to Scan a QR code:
		IntentIntegrator qrIntentIntegrator = new IntentIntegrator(this);
		qrIntentIntegrator.setTitleByID(R.string.qrInstallScannerTtl);
		qrIntentIntegrator.setMessageByID(R.string.qrInstallScannerMsg);
		qrIntentIntegrator.setButtonYesByID(R.string.yes);
		qrIntentIntegrator.setButtonNoByID(R.string.no);
		qrIntentIntegrator.initiateScan(IntentIntegrator.QR_CODE_TYPES);
		
		closeDrawer(null);
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent intent)
	{
		super.onActivityResult(requestCode, resultCode, intent);

		if(resultCode != Activity.RESULT_OK)
		{
			//Log.d(TAG, "onActivityResult() called with non-OK resultCode (requestCode: " + requestCode + "; resultCode: " + resultCode + ")!");
			return;
		}
		//else...
		Uri uri = intent.getData();
		boolean skipRefreshOnMainTabResume = false;
		switch(requestCode)
		{
			// File browse dialog for project loading:
			case RETURN_BROWSE_FOR_PROJECT_LOAD:
				skipRefreshOnMainTabResume = true; // see explanation below
				// Get the File path from the Uri
				try
				{
					loadProject(FileUtils.getPath(this, uri));
				}
				catch(Exception e)
				{
					Toast.makeText(this, "Cannot load: " + uri, Toast.LENGTH_LONG).show();
				}
				break;

			// File browse dialog for record importing:
			case RETURN_BROWSE_FOR_RECORD_IMPORT:
				skipRefreshOnMainTabResume = true; // see explanation below
				importFrom(FileUtils.getFile(this, uri));
				break;
				
			// QR Reader
			case IntentIntegrator.REQUEST_CODE:
				IntentResult scanResult = IntentIntegrator.parseActivityResult(requestCode, resultCode, intent);
				if(scanResult != null)
				{
					skipRefreshOnMainTabResume = true; // see explanation below
					loadProject(scanResult.getContents());
				}
				break;
		}
		
		/* Note: after onActivityResult() the activity's and each visible tab's onResume() method will
		 * be called. In the case of the MainTabFragment this would normally trigger a refreshing of
		 * the project data stats, which makes no sense if it happens before/during project loading or
		 * data importing (or twice). Hence we tell the MainTabFragment no to refresh itself upon the
		 * next onResume() call: */
		if(skipRefreshOnMainTabResume)
			try
			{
				pagerAdapter.getTab(MainTabFragment.class).setSkipRefreshOnNextResume(true);
			}
			catch(Exception ignore) {}
	}

	public void importFrom(File exportedDataFile)
	{
		try
		{
			String extension = FileHelpers.getFileExtension(exportedDataFile).toLowerCase();
			switch(extension)
			{
				case XMLRecordsExporter.FILE_EXTENSION :
					new RecordsTasks.XMLImportTask(this, this).execute(exportedDataFile);
					break;
				case CSVRecordsExporter.FILE_EXTENSION :
					new RecordsTasks.CSVImportTask(this, this).execute(exportedDataFile);
					break;
				default :
					showErrorDialog(getString(R.string.unknownExportExtension, extension), false);
			}
		}
		catch(Exception e)
		{
			showErrorDialog(getString(R.string.importStartFailed, ExceptionHelpers.getMessageAndCause(e)), false);
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public void importSuccess(List<Record> records, List<String> warnings)
	{
		// Show parser warnings if needed:
		if(!warnings.isEmpty())
			showWarningDialog(listWarnings(R.string.parsingWarnings, warnings));
		// Store the records:
		new RecordsTasks.StoreTask(this, new RecordsTasks.StoreCallback()
		{
			@Override
			public void storeSuccess(int newRecords, int updatedRecords, int skippedDuplicates)
			{
				showInfoDialog(getString(R.string.importSuccess, newRecords, updatedRecords, skippedDuplicates));
				onDataChanged();
			}
			
			@Override
			public void storeFailure(Exception reason)
			{
				showErrorDialog(getString(R.string.importStoreFailed, reason.getMessage()), false);
				onDataChanged();
			}
		}).execute(records);
	}

	@Override
	public void importFailure(Exception reason)
	{
		if(reason instanceof UnknownModelException)
		{
			UnknownModelException ume = (UnknownModelException) reason;
			showErrorDialog(
				getString(
					R.string.importFailed,
					(ume.getSchemaName() != null ?
						getString(
							R.string.schemaFormNotFound,
							ume.getSchemaName()) :
						ume.getMessage())),
				false);
		}
		else
			showErrorDialog(getString(R.string.importFailed, reason.getMessage()), false);
	}

	/**
	 * Menu action which removes project if user confirms this is what he/she wants.
	 * 
	 * @param item
	 */
	public void removeProject(MenuItem item)
	{
		removeProject(getCurrentProject(false), true);
	}
	
	/**
	 * Removes the given project, optionally after asking for confirmation.
	 * 
	 * @param projDesc
	 * @param askConfirmation
	 */
	public void removeProject(final ProjectDescriptor projDesc, boolean askConfirmation)
	{
		if(projDesc == null)
			return;
		if(askConfirmation)		
			showYesNoDialog(R.string.remove_project, getString(R.string.removeProjectConfirm, projDesc.toString(false)), R.drawable.ic_delete_black_36dp, new Runnable()
			{
				@Override
				public void run()
				{
					removeProject(projDesc);
				}
			}, false, null, false);
		else
			removeProject(projDesc); // remove straight away
	}
	
	/**
	 * Removes the given project. Does *not* ask for confirmation.
	 * Use with care!
	 * 
	 * @param projDesc
	 */
	public void removeProject(final ProjectDescriptor projDesc)
	{
		if(projDesc != null)
			new ProjectTasks.RemoveProjectTask(ProjectManagerActivity.this, projectStore, new ProjectTasks.RemoveProjectCallback()
			{
				@Override
				public void projectRemoved()
				{
					updateProjectList(true); // Refresh list
				}
			}).execute(projDesc);
	}

	/**
	 * @return the projectStore
	 */
	public ProjectStore getProjectStore()
	{
		return projectStore;
	}
	
	/**
	 * @return the transmissionStore
	 */
	public TransmissionStore getTransmissionStore()
	{
		return projectStore.getTransmissionStore(); // use the transmissionStore instance used by the ProjectStore
	}

	private String listWarnings(int titleStringId, List<String> warnings)
	{
		if(warnings.isEmpty())
			return null;
		//else:
		TransactionalStringBuilder bldr = new TransactionalStringBuilder("\n");
		bldr.append(getString(titleStringId) + ":");
		for(String warning : warnings)
			bldr.append(" - " + warning);
		return bldr.toString();
	}

}