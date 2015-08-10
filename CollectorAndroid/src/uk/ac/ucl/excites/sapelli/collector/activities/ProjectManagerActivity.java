/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
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

import java.io.File;
import java.util.Collections;
import java.util.List;

import com.astuetz.PagerSlidingTabStrip;
import com.crashlytics.android.Crashlytics;
import com.ipaulpro.afilechooser.utils.FileUtils;

import android.app.Activity;
import android.app.Dialog;
import android.content.ActivityNotFoundException;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.os.Debug;
import android.support.v4.view.ViewPager;
import android.support.v4.widget.DrawerLayout;
import android.support.v7.app.ActionBar;
import android.support.v7.app.ActionBarDrawerToggle;
import android.support.v7.widget.Toolbar;
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
import uk.ac.ucl.excites.sapelli.collector.BuildConfig;
import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.fragments.AboutFragment;
import uk.ac.ucl.excites.sapelli.collector.fragments.EnterURLFragment;
import uk.ac.ucl.excites.sapelli.collector.fragments.ExportFragment;
import uk.ac.ucl.excites.sapelli.collector.load.AndroidProjectLoaderStorer;
import uk.ac.ucl.excites.sapelli.collector.load.ProjectLoader;
import uk.ac.ucl.excites.sapelli.collector.load.ProjectLoaderStorer;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.services.DataSendingSchedulingService;
import uk.ac.ucl.excites.sapelli.collector.model.ProjectDescriptor;
import uk.ac.ucl.excites.sapelli.collector.tasks.Backup;
import uk.ac.ucl.excites.sapelli.collector.tasks.ProjectTasks;
import uk.ac.ucl.excites.sapelli.collector.tasks.RecordsTasks;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendingSchedule;
import uk.ac.ucl.excites.sapelli.collector.ui.ProjectManagerPagerAdapter;
import uk.ac.ucl.excites.sapelli.collector.util.AsyncDownloader;
import uk.ac.ucl.excites.sapelli.collector.util.AsyncTaskWithWaitingDialog;
import uk.ac.ucl.excites.sapelli.collector.util.DeviceID;
import uk.ac.ucl.excites.sapelli.collector.util.ProjectRunHelpers;
import uk.ac.ucl.excites.sapelli.collector.util.qrcode.IntentIntegrator;
import uk.ac.ucl.excites.sapelli.collector.util.qrcode.IntentResult;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle.StoreUser;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.shared.util.TransactionalStringBuilder;
import uk.ac.ucl.excites.sapelli.shared.util.android.MenuHelpers;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;
import uk.ac.ucl.excites.sapelli.transmission.model.Correspondent;
import uk.ac.ucl.excites.sapelli.transmission.model.transport.sms.SMSCorrespondent;

/**
 * @author Julia, Michalis Vitos, mstevens
 * 
 */
public class ProjectManagerActivity extends BaseActivity implements StoreUser, DeviceID.InitialisationCallback, ProjectLoaderStorer.FileSourceCallback, AsyncDownloader.Callback, ListView.OnItemClickListener, RecordsTasks.ImportCallback
{

	// STATICS--------------------------------------------------------
	static private final String TAG = "ProjectManagerActivity";

	static protected final String XML_FILE_EXTENSION = "xml";

	static protected final String DEMO_PROJECT = "demo.excites";

	public static final int RETURN_BROWSE_FOR_PROJECT_LOAD = 1;
	public static final int RETURN_BROWSE_FOR_IMMEDIATE_PROJECT_LOAD = 2;
	public static final int RETURN_BROWSE_FOR_RECORD_IMPORT = 3;
	
	private static final int PAGER_MARGIN_DIP = 4;

	// DYNAMICS-------------------------------------------------------
	private DeviceID deviceID;
	private ProjectStore projectStore;
	private Project currentProject;
	
	private ArrayAdapter<ProjectDescriptor> projectListAdaptor;

	// UI
	private TextView addProjects;
	private PagerSlidingTabStrip tabs;
	private ViewPager pager;
	private Dialog encryptionDialog;
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
		
		if(app.getBuildInfo().isDemoBuild())
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
		tabs = (PagerSlidingTabStrip) findViewById(R.id.tabs);
		pager = (ViewPager) findViewById(R.id.pager);
		pager.setPageMargin((int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, PAGER_MARGIN_DIP, getResources().getDisplayMetrics()));
		addProjects = (TextView) findViewById(R.id.addProjects);
		
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
		menu.findItem(R.id.action_remove).setVisible(projectListAdaptor != null && projectListAdaptor.getCount() > 0);

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
			projectStore = app.collectorClient.projectStoreHandle.getStore(this);
		}
		catch(Exception e)
		{
			Log.e(TAG, getString(R.string.projectStorageAccessFail), e);
			showErrorDialog(getString(R.string.projectStorageAccessFail, ExceptionHelpers.getMessageAndCause(e)), true);
			return;
		}
		
		// And finally...
		/*if(app.getBuildInfo().isDemoBuild())
			demoMode();
		else*/
		updateProjectList(false);
		
		// stop tracing
		Debug.stopMethodTracing();
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
		if(!BuildConfig.DEBUG)
		{
			Crashlytics.setLong(CollectorApp.CRASHLYTICS_DEVICE_ID_CRC32, deviceID.getIDAsCRC32Hash());
			Crashlytics.setString(CollectorApp.CRASHLYTICS_DEVICE_ID_MD5, deviceID.getIDAsMD5Hash().toString());
		}
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
	
	private class RetrieveProjectDescriptorsTask extends AsyncTaskWithWaitingDialog<Void, List<ProjectDescriptor>>
	{
		
		public RetrieveProjectDescriptorsTask()
		{
			super(ProjectManagerActivity.this, getString(R.string.load_projects));
		}

		@Override
		protected List<ProjectDescriptor> doInBackground(Void... params)
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
			projectListAdaptor = new ArrayAdapter<ProjectDescriptor>(ProjectManagerActivity.this, R.layout.projectlist_item, result);
			projectList.setAdapter(projectListAdaptor);
			
			// Set label above list:
			lblAvailableProjects.setText(result.isEmpty() ? R.string.no_projects : R.string.switch_project);
			
			// Set/switch to current project
			if(currentProject != null)
			{	// look for current project in result list:
				for(int i = 0; i < result.size(); i++)
					if(result.get(i).equalDescription(currentProject))
						// (Re)select current project:
						projectList.setItemChecked(i, true); // no need to call switchToProject(), project is already the current one
			}
			else if(app.getPreferences().getActiveProjectSignature() != null)
			{	// Reselect previously active project:
				String prevActiveProjectSign = app.getPreferences().getActiveProjectSignature();
				for(int i = 0; i < result.size(); i++)
					if(result.get(i).getSignatureString().equals(prevActiveProjectSign))
					{
						// select in list:
						projectList.setItemChecked(i, true);
						// switch to it:
						switchToProject(result.get(i));
					}
			}
			else
				// select nothing or first project in list:
				switchToProject(result.isEmpty() ? null : result.get(0)); 
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
	
	private void switchToProject(ProjectDescriptor projDescr)
	{		
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
		pager.setAdapter(new ProjectManagerPagerAdapter(this, getSupportFragmentManager()));
		if(currentProject != null)
		{
			getSupportActionBar().setTitle(currentProject.getName());
			getSupportActionBar().setSubtitle(currentProject.getVariantVersionString());
			tabs.setViewPager(pager);
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

		// Remember current project:
		app.getPreferences().setActiveProjectSignature(currentProject);
		// Refresh menus:
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
	 * Click on the big '+' actionbar button
	 * 
	 * @param menuItem
	 */
	public void browse(MenuItem menuItem)
	{
		browse(true);
		closeDrawer(null);
	}
	
	public void browse(boolean loadImmediately)
	{
		// Use the GET_CONTENT intent from the utility class
		Intent target = FileUtils.createGetContentIntent();
		// Create the chooser Intent
		Intent intent = Intent.createChooser(target, getString(R.string.chooseSapelliFile));
		try
		{
			// if view == null this means we've been called from loadProject(), i.e. the user has clicked "Load" instead of "Browse":
			startActivityForResult(intent, loadImmediately ? RETURN_BROWSE_FOR_IMMEDIATE_PROJECT_LOAD : RETURN_BROWSE_FOR_PROJECT_LOAD);
		}
		catch(ActivityNotFoundException e){}
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
		// clean up:
		app.collectorClient.projectStoreHandle.doneUsing(this); // signal that the activity no longer needs the DAO
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
		// TODO START HACK HACK
		Log.d(TAG, "Starting alarm scheduler...");
		DataSendingSchedulingService.ScheduleAll(getApplicationContext());
		/// TODO END HACK HACK
		
		openAboutDialog();
		return true;
	}
	
	private void openAboutDialog()
	{
		new AboutFragment().show(getSupportFragmentManager(), getString(R.string.about));
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

	public boolean importRecords(MenuItem item)
	{
		// Use the GET_CONTENT intent from the utility class
		Intent target = FileUtils.createGetContentIntent();
		// Create the chooser Intent
		Intent intent = Intent.createChooser(target, "Choose an XML file");
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
		Backup.Run(this, fileStorageProvider);
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
		if(path == null || path.isEmpty())
			return;
		//else:
		String location = path.trim();
		// Download Sapelli file if path is a URL
		if(Patterns.WEB_URL.matcher(location).matches())
			// Location is a (remote) URL: download Sapelli file:
			AsyncDownloader.Download(this, fileStorageProvider.getSapelliDownloadsFolder(), location, this); // loading & store of the project will happen upon successful download (via callback)
		else if(location.toLowerCase().endsWith("." + XML_FILE_EXTENSION))
			// Warn about bare XML file (no longer supported):
			showErrorDialog(R.string.noBareXMLProjects);
		else
		{	// loading project from local file:
			File localFile = new File(location);
			if(ProjectLoader.HasSapelliFileExtension(localFile))
				new AndroidProjectLoaderStorer(this, fileStorageProvider, projectStore).loadAndStore(localFile, Uri.fromFile(localFile).toString(), this);
			else
				showErrorDialog(getString(R.string.unsupportedExtension, FileHelpers.getFileExtension(localFile), StringUtils.join(ProjectLoader.SAPELLI_FILE_EXTENSIONS, ", ")));
		}
	}
	
	public void scanQR(MenuItem item)
	{
		// Start the Intent to Scan a QR code
		new IntentIntegrator(this).initiateScan(IntentIntegrator.QR_CODE_TYPES);
		
		closeDrawer(null);
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent intent)
	{
		super.onActivityResult(requestCode, resultCode, intent);

		if(resultCode != Activity.RESULT_OK)
			return;
		//else...
		Uri uri = intent.getData();
		switch(requestCode)
		{
			// File browse dialog for project loading:
			case RETURN_BROWSE_FOR_PROJECT_LOAD:
			case RETURN_BROWSE_FOR_IMMEDIATE_PROJECT_LOAD:
				// Get the File path from the Uri
				loadProject(FileUtils.getPath(this, uri));
				break;

			// File browse dialog for record importing:
			case RETURN_BROWSE_FOR_RECORD_IMPORT:
				new RecordsTasks.XMLImportTask(this, this).execute(FileUtils.getFile(this, uri)); // run XMLImportTask ...
				break;
				
			// QR Reader
			case IntentIntegrator.REQUEST_CODE:
				IntentResult scanResult = IntentIntegrator.parseActivityResult(requestCode, resultCode, intent);
				if (scanResult != null) {
					String fileUrl = intent.getStringExtra("SCAN_RESULT");
					loadProject(fileUrl);
				}
				break;
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public void importSuccess(List<Record> records, List<String> warnings)
	{
		// Show parser warnings if needed: 
		if(!warnings.isEmpty())
		{
			TransactionalStringBuilder bldr = new TransactionalStringBuilder("\n");
			bldr.append(getString(R.string.parsingWarnings) + ":");
			for(String warning : warnings)
				bldr.append(" - " + warning);
			showWarningDialog(bldr.toString());
		}
		// Store the records:
		new RecordsTasks.StoreTask(this, new RecordsTasks.StoreCallback()
		{
			@Override
			public void storeSuccess(List<Record> storedRecords)
			{
				showInfoDialog("Succesfully imported " + storedRecords.size() + " records."); // TODO report skipped duplicates
			}
			
			@Override
			public void storeFailure(Exception reason)
			{
				showErrorDialog("Error upon storing imported records: " + reason.getMessage(), false);
			}
		}).execute(records);
	}

	@Override
	public void importFailure(Exception reason)
	{
		try
		{
			throw reason;
		}
		catch(UnknownModelException ume)
		{
			showErrorDialog("Error upon importing records: " + (ume.getSchemaName() != null ? "could not find matching project/form (Project:Form = " + ume.getSchemaName() + ")" : ume.getMessage()));
		}
		catch(Exception e)
		{
			showErrorDialog("Error upon importing records: " + e.getMessage(), false);
		}
	}

	/**
	 * Dialog to check whether it is desired to remove project
	 * 
	 * @param view
	 */
	public void removeProject(MenuItem item)
	{
		Project project = getCurrentProject(true);
		if(project != null)
		{
			showYesNoDialog(R.string.remove_project, getString(R.string.removeProjectConfirm, project.toString(false)), R.drawable.ic_delete_black_36dp, new Runnable()
			{
				@Override
				public void run()
				{
					new ProjectTasks.RemoveProjectTask(ProjectManagerActivity.this, projectStore, new ProjectTasks.RemoveProjectCallback()
					{
						@Override
						public void projectRemoved()
						{
							updateProjectList(true); // Refresh list
						}
					}).execute(getCurrentProject(false));
				}
			}, false, null, false);
		}
	}
	
	@Override
	protected void onPause()
	{
		super.onPause();
		if(encryptionDialog != null)
			encryptionDialog.dismiss();
	}
	
	@Override
	public void downloadSuccess(String downloadUrl, File downloadedFile)
	{
		new AndroidProjectLoaderStorer(this, fileStorageProvider, projectStore).loadAndStore(downloadedFile, downloadUrl, this);
	}

	@Override
	public void downloadFailure(String downloadUrl, Exception cause)
	{
		showErrorDialog(R.string.downloadError, false);
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
		{
			bldr.append(getString(R.string.projectLoadingWarnings) + ":");
			for(String warning : warnings)
				bldr.append(" - " + warning);
		}
		//	Check file dependencies:
		List<String> missingFiles = project.getMissingFilesRelativePaths(fileStorageProvider);
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

		// TODO ---------------- delete below
		try
		{
			// once project loading done, store a dummy schedule:
			Correspondent receiver = new SMSCorrespondent("Matthias Belgium", "+32486170492", false);
			TransmissionStore sentTxStore = ((CollectorApp)this.getApplication()).collectorClient.transmissionStoreHandle.getStore(this);
			sentTxStore.store(receiver);
			projectStore.storeSendSchedule(new SendingSchedule(project, receiver, 60 /*seconds*/, false), sentTxStore);
		}
		catch(Exception e)
		{
			e.printStackTrace();
		}
		// TODO ---------------- delete above
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

}