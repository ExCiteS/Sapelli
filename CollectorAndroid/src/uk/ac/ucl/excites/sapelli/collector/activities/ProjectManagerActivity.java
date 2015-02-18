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
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.BuildConfig;
import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.load.AndroidProjectLoaderStorer;
import uk.ac.ucl.excites.sapelli.collector.load.ProjectLoader;
import uk.ac.ucl.excites.sapelli.collector.load.ProjectLoaderStorer;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.tasks.Backup;
import uk.ac.ucl.excites.sapelli.collector.util.AsyncDownloader;
import uk.ac.ucl.excites.sapelli.collector.util.DeviceID;
import uk.ac.ucl.excites.sapelli.collector.util.ProjectRunHelpers;
import uk.ac.ucl.excites.sapelli.collector.util.qrcode.IntentIntegrator;
import uk.ac.ucl.excites.sapelli.collector.util.qrcode.IntentResult;
import uk.ac.ucl.excites.sapelli.shared.db.StoreHandle;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.shared.util.TransactionalStringBuilder;
import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsImporter;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.ActivityNotFoundException;
import android.content.Intent;
import android.content.res.AssetManager;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.text.Html;
import android.text.method.LinkMovementMethod;
import android.util.Log;
import android.util.Patterns;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuItem;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.WindowManager;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.TextView;

import com.crashlytics.android.Crashlytics;
import com.ipaulpro.afilechooser.utils.FileUtils;
import com.larvalabs.svgandroid.SVG;
import com.larvalabs.svgandroid.SVGBuilder;
import com.larvalabs.svgandroid.SVGDrawable;

/**
 * @author Julia, Michalis Vitos, mstevens
 * 
 */
public class ProjectManagerActivity extends BaseActivity implements StoreHandle.StoreUser, DeviceID.InitialisationCallback, ProjectLoaderStorer.FileSourceCallback, AsyncDownloader.Callback
{

	// STATICS--------------------------------------------------------
	static private final String TAG = "ProjectManagerActivity";
	
	static protected final String XML_FILE_EXTENSION = "xml";

	static private final String DEMO_PROJECT = "demo.excites";

	public static final int RETURN_BROWSE_FOR_PROJECT_LOAD = 1;
	public static final int RETURN_BROWSE_FOR_RECORD_IMPORT = 2;

	// DYNAMICS-------------------------------------------------------
	private ProjectStore projectStore;

	// UI
	private EditText txtProjectPathOrURL;
	private ListView projectList;
	private Button runBtn;
	private Button removeBtn;
	private Dialog encryptionDialog;
	private DeviceID deviceID;

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		
		if(app.getBuildInfo().isDemoBuild())
			return;
		//else ...
		// Only if not in demo mode:
		
		// Set-up UI...
		setTitle(getString(R.string.app_name) + ' ' + getString(R.string.project_manager));
		// Hide soft keyboard on create
		getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_HIDDEN);
		setContentView(R.layout.activity_projectmanager);
		// Get View Elements
		txtProjectPathOrURL = (EditText) findViewById(R.id.txtProjectPathOrURL);
		projectList = (ListView) findViewById(R.id.ProjectsList);
		runBtn = (Button) findViewById(R.id.RunProjectButton);
		removeBtn = (Button) findViewById(R.id.RemoveProjectButton);
		// Set background logo under project list:
		if(android.os.Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT /* = 19 */)
			try	// For some reason the background image blows up the size of the containing RelativeLayout on Android < KitKat. TODO investigate further & ask on StackOverflow and/or contact AndroidSVG creator
			{
				ImageView projListBackground = (ImageView) findViewById(R.id.ProjectsListBackgroundImage);
				SVG svg = new SVGBuilder().readFromResource(getResources(), R.drawable.sapelli_logo).build();
				projListBackground.setImageDrawable(new SVGDrawable(svg));
				//SVG svg = SVG.getFromResource(getResources(), R.drawable.sapelli_logo);
				//projListBackground.setImageDrawable(new PictureDrawable(svg.renderToPicture()));
			}
			catch(Exception e)
			{
				e.printStackTrace(System.err);
			}
		// Make title bar click open the about dialog:
		View v = findViewById (android.R.id.title);
	    v.setClickable(true);
	    v.setOnClickListener(new OnClickListener()
	    {
	        @Override
	        public void onClick(View v)
	        {
	        	openAboutDialog(null);
	        }
	    });
		// Get scrolling right
		findViewById(R.id.projectManager_ScrollView).setOnTouchListener(new View.OnTouchListener()
		{
			@SuppressLint("ClickableViewAccessibility")
			@Override
			public boolean onTouch(View v, MotionEvent event)
			{
				projectList.getParent().requestDisallowInterceptTouchEvent(false);
				return false;
			}
		});
		projectList.setOnTouchListener(new View.OnTouchListener()
		{
			@SuppressLint("ClickableViewAccessibility")
			public boolean onTouch(View v, MotionEvent event)
			{	// Disallow the touch request for parent scroll on touch of child view
				v.getParent().requestDisallowInterceptTouchEvent(true);
				return false;
			}
		});

/*		// TODO Re-enable the service at same point
		// Check the Preferences
		if(DataSenderPreferences.getTimeSchedule(this) == 1)
		{
			DataSenderPreferences.printPreferences(this);
			Toast.makeText(this, "Please configure the Data Sender.", Toast.LENGTH_LONG).show();
			
			Intent settingsActivity = new Intent(this, DataSenderPreferences.class);
			startActivity(settingsActivity);
		}
		
		// Start the DataSenderService
		if(DataSenderPreferences.getSenderEnabled(this)) //TODO make this optional
			ServiceChecker.startService(this);
*/
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
			showErrorDialog(getString(R.string.projectStorageAccessFail, ExceptionHelpers.getMessageAndCause(e)), true);
			return;
		}
		
		// And finally...
		if(app.getBuildInfo().isDemoBuild())
			demoMode();
		else
			populateProjectList(); 	// Update project list
		// TODO remember & re-select last selected project
	}
	
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

	private void demoMode()
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
	}

	@Override
	protected void onDestroy()
	{
		// clean up:
		app.collectorClient.projectStoreHandle.doneUsing(this); // signal that the activity no longer needs the DAO
		// super:
		super.onDestroy();
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu)
	{
		super.onCreateOptionsMenu(menu);
		getMenuInflater().inflate(R.menu.projectmanager, menu);
		return true;
	}
	
	@Override
	public boolean onOptionsItemSelected(MenuItem item)
	{
		/* 	Note:
		 * 	the android:onClick attribute in the XML only works on Android >= v3.0,
		 *	so we need to direct the handling of menu clicks manually here for things
		 *	to work on earlier versions. */
	    switch(item.getItemId())
	    {
	    	case R.id.sender_settings_menuitem :
	    		return openSenderSettings(item);
	    	case R.id.export_records_menuitem :
	    		return exportRecords(item);
	    	//case R.id.import_records_menuitem :
	    		//return importRecords(item);
	    	case R.id.create_shortcut :
	    		return createShortcut(item);
	    	case R.id.remove_shortcut :
	    		return removeShortcut(item);
			case R.id.backup:
				return backupSapelli(item);
	    	case R.id.about_menuitem :
	    		return openAboutDialog(item);
	    }
	    return true;
	}

	public boolean openSenderSettings(MenuItem item)
	{
		// TODO Re-enable the service at same point
		// startActivity(new Intent(getBaseContext(), DataSenderPreferences.class));
		return true;
	}

	@SuppressLint("InflateParams")
	public boolean openAboutDialog(MenuItem item)
	{
		// Set-up UI:
		View view = LayoutInflater.from(this).inflate(R.layout.dialog_about, null);
		TextView infoLbl = (TextView) view.findViewById(R.id.aboutInfo);
		infoLbl.setClickable(true);
		infoLbl.setMovementMethod(LinkMovementMethod.getInstance());
		infoLbl.setText(Html.fromHtml(
				"<p><b>" + app.getBuildInfo().getNameAndVersion() + "</b><br/>[" + app.getBuildInfo().getExtraVersionInfo() + "]</p>" +
				"<p>" + app.getBuildInfo().getBuildInfo() + ".</p>" +
				"<p>" + getString(R.string.by_ucl_excites_html)  + "</p>" + 
				"<p>" + getString(R.string.license)  + "</p>" +
				"<p>" + "Device ID (CRC32): " + (deviceID != null ? deviceID.getIDAsCRC32Hash() : "?") + ".</p>"));	
		infoLbl.setPadding(2, 2, 6, 2);
		ImageView iconImg = (ImageView) view.findViewById(R.id.aboutIcon);
		iconImg.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				startActivity(new Intent(Intent.ACTION_VIEW, Uri.parse("http://sapelli.org")));
			}
		});
		
		// Set-up dialog:
		AlertDialog.Builder dialogBuilder = new AlertDialog.Builder(this);
		dialogBuilder.setPositiveButton(getString(android.R.string.ok), null); // click will dismiss the dialog (BACK press will too)
		AlertDialog aboutDialog = dialogBuilder.create();
		aboutDialog.setView(view, 0, 0, 0, 0); // no margins

		// Show the dialog:
		aboutDialog.show();
		
		return true;
	}
	
	public boolean exportRecords(MenuItem item)
	{
		Project selectedProject = getSelectedProject(false);
		Intent i = new Intent(getApplicationContext(), ExportActivity.class);
		if(selectedProject != null)
		{
			i.putExtra(CollectorActivity.INTENT_PARAM_PROJECT_ID, selectedProject.getID());
			i.putExtra(CollectorActivity.INTENT_PARAM_PROJECT_FINGERPRINT, selectedProject.getFingerPrint());
		}
		i.setAction(Intent.ACTION_MAIN);
		startActivity(i);
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
	
	public void browse(View view)
	{
		// Use the GET_CONTENT intent from the utility class
		Intent target = FileUtils.createGetContentIntent();
		// Create the chooser Intent
		Intent intent = Intent.createChooser(target, getString(R.string.chooseSapelliFile));
		try
		{
			startActivityForResult(intent, RETURN_BROWSE_FOR_PROJECT_LOAD);
		}
		catch(ActivityNotFoundException e){}
	}

	/**
	 * Retrieve all parsed projects from db and populate list
	 */
	private void populateProjectList()
	{
		projectList.setAdapter(new ArrayAdapter<Project>(this, R.layout.project_list_item, android.R.id.text1, projectStore.retrieveProjects()));
		if(!projectList.getAdapter().isEmpty())
		{
			runBtn.setEnabled(true);
			removeBtn.setEnabled(true);
			projectList.setItemChecked(0, true); // check first project in the list
		}
		else
		{
			runBtn.setEnabled(false);
			removeBtn.setEnabled(false);
		}
	}

	@SuppressWarnings("unchecked")
	protected void selectProjectInList(Project project)
	{
		projectList.setItemChecked(((ArrayAdapter<Project>) projectList.getAdapter()).getPosition(project), true);
	}

	@SuppressWarnings("unchecked")
	protected Project getSelectedProject(boolean errorIfNull)
	{
		if(projectList.getCheckedItemPosition() == -1)
		{
			if(errorIfNull)
				showErrorDialog(R.string.selectProject, false);
			return null;
		}
		return ((ArrayAdapter<Project>) projectList.getAdapter()).getItem(projectList.getCheckedItemPosition());
	}

	public void runProject(View view)
	{
		Project p = getSelectedProject(true);
		if(p != null)
			startActivity(ProjectRunHelpers.getProjectRunIntent(this, p));
	}

	private void removeProject()
	{
		Project project = getSelectedProject(false);
		if(project == null)
			return;
		
		// Remove project from store:
		projectStore.delete(project);
		
		// Remove installation folder:
		org.apache.commons.io.FileUtils.deleteQuietly(fileStorageProvider.getProjectInstallationFolder(project, false));
		
		// Remove shortcut:
		ProjectRunHelpers.removeShortcut(this, project);
		
		// Refresh list:
		populateProjectList();

		// TODO Re-enable the service at same point
		// Restart the DataSenderService to stop monitoring the deleted project
		// ServiceChecker.restartActiveDataSender(this);
	}

	public void loadProject(View view)
	{
		String location = txtProjectPathOrURL.getText().toString().trim();
		if(location.isEmpty())
			// Download Sapelli file if path is a URL
			showErrorDialog(R.string.pleaseSelect);
		else
		{
			// Extract & parse a local Sapelli file
			txtProjectPathOrURL.setText("");

			// Add project
			if(Patterns.WEB_URL.matcher(location).matches())
				// Location is a (remote) URL: download Sapelli file:
				AsyncDownloader.Download(this, fileStorageProvider.getSapelliDownloadsFolder(), location, this); // loading & store of the project will happen upon successful download (via callback)
			else if(location.toLowerCase().endsWith("." + XML_FILE_EXTENSION))
				// Warn about bare XML file (no longer supported):
				showErrorDialog(R.string.noBareXMLProjects);
			else
			{
				File localFile = new File(location);
				if(ProjectLoader.HasSapelliFileExtension(localFile))
					// Load & store project from local file:
					new AndroidProjectLoaderStorer(this, fileStorageProvider, projectStore).loadAndStore(localFile, Uri.fromFile(localFile).toString(), this);
				else
					showErrorDialog(getString(R.string.unsupportedExtension, FileHelpers.getFileExtension(localFile),
							StringUtils.join(ProjectLoader.SAPELLI_FILE_EXTENSIONS, ", ")));
				// Use the path where the xml file resides as the basePath (img&snd folders are assumed to be in the same place), no subfolders are created:
				// Show parser warnings if needed:
			}
		}
	}
	
	public void scanQR(View view)
	{
		// Start the Intent to Scan a QR code
		IntentIntegrator integrator = new IntentIntegrator(this);
		integrator.initiateScan(IntentIntegrator.QR_CODE_TYPES);
	}
	
	/**
	 * Create a shortcut
	 * 
	 */
	public boolean createShortcut(MenuItem item)
	{
		// Get the selected project
		Project selectedProject = getSelectedProject(true);
		if(selectedProject != null)
			ProjectRunHelpers.createShortcut(this, fileStorageProvider, selectedProject);
		return true;
	}

	/**
	 * Remove a shortcut
	 * 
	 */
	public boolean removeShortcut(MenuItem item)
	{
		// Get the selected project
		Project selectedProject = getSelectedProject(true);
		if(selectedProject != null)
			ProjectRunHelpers.removeShortcut(this, selectedProject);
		return true;
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data)
	{
		super.onActivityResult(requestCode, resultCode, data);

		Uri uri = null;
		String path = null;

		if(resultCode == Activity.RESULT_OK)
			switch(requestCode)
			{
			// File browse dialog for project loading:
			case RETURN_BROWSE_FOR_PROJECT_LOAD:

				uri = data.getData();

				// Get the File path from the Uri
				path = FileUtils.getPath(this, uri);

				// Alternatively, use FileUtils.getFile(Context, Uri)
				if(path != null && FileUtils.isLocal(path))
				{
					txtProjectPathOrURL.setText(path);
					// Move the cursor to the end
					txtProjectPathOrURL.setSelection(path.length());
				}

				break;

			// File browse dialog for record importing:
			case RETURN_BROWSE_FOR_RECORD_IMPORT:

				uri = data.getData();

				// Get the File path from the Uri
				path = FileUtils.getPath(this, uri);

				// Alternatively, use FileUtils.getFile(Context, Uri)
				if(path != null && FileUtils.isLocal(path))
				{
					try
					{ // TODO make import & storage async
						// Import:
						XMLRecordsImporter importer = new XMLRecordsImporter(app.collectorClient);
						List<Record> records = importer.importFrom((new File(path)).getAbsoluteFile());

						// Show parser warnings if needed:
						List<String> warnings = importer.getWarnings(); 
						if(!warnings.isEmpty())
						{
							TransactionalStringBuilder bldr = new TransactionalStringBuilder("\n");
							bldr.append(getString(R.string.parsingWarnings) + ":");
							for(String warning : warnings)
								bldr.append(" - " + warning);
							showWarningDialog(bldr.toString());
						}

						/*//TEST CODE (export again to compare with imported file):
						RecordsExporter exporter = new RecordsExporter(((CollectorApp) getApplication()).getDumpFolderPath(), dao);
						exporter.export(records);*/
	
						//Store the records:
						//for(Record r : records)
						//	dao.store(r); //TODO avoid duplicates!
						
						//User feedback:
						showInfoDialog("Succesfully imported " + records.size() + " records."); //TODO report skipped duplicates
					}
					catch(Exception e)
					{
						showErrorDialog("Error upon importing records: " + e.getMessage(), false);
					}
				}

				break;
			// QR Reader
			case IntentIntegrator.REQUEST_CODE:
				IntentResult scanResult = IntentIntegrator.parseActivityResult(requestCode, resultCode, data);
				if(scanResult != null)
				{
					String fileUrl = data.getStringExtra("SCAN_RESULT");
					txtProjectPathOrURL.setText(fileUrl);
					// Move the cursor to the end
					txtProjectPathOrURL.setSelection(fileUrl.length());
				}
				break;
			}
	}

	/**
	 * Dialog to check whether it is desired to remove project
	 * 
	 * @param view
	 */
	public void removeDialog(View view)
	{
		Project project = getSelectedProject(true);
		if(project != null)
		{
			showYesNoDialog(R.string.project_manager, R.string.removeProjectConfirm, false, new Runnable()
			{
				@Override
				public void run()
				{
					removeProject();
				}
			}, false);
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
		
		// Update project list:
		populateProjectList();
		selectProjectInList(project); // select the new project

		// TODO Re-enable the service at same point
		// Restart the DataSenderService to start monitoring the new project
		// ServiceChecker.restartActiveDataSender(this);
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