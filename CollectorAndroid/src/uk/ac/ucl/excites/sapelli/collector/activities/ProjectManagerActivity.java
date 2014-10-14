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

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.apache.commons.lang3.ArrayUtils;

import uk.ac.ucl.excites.sapelli.collector.BuildConfig;
import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.db.exceptions.ProjectDuplicateException;
import uk.ac.ucl.excites.sapelli.collector.io.FileStorageProvider.Folders;
import uk.ac.ucl.excites.sapelli.collector.io.ProjectLoader;
import uk.ac.ucl.excites.sapelli.collector.io.ProjectLoaderCallback;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.util.AsyncZipper;
import uk.ac.ucl.excites.sapelli.collector.util.DeviceID;
import uk.ac.ucl.excites.sapelli.collector.util.ProjectRunHelpers;
import uk.ac.ucl.excites.sapelli.collector.util.qrcode.IntentIntegrator;
import uk.ac.ucl.excites.sapelli.collector.util.qrcode.IntentResult;
import uk.ac.ucl.excites.sapelli.shared.db.StoreClient;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;
import uk.ac.ucl.excites.sapelli.shared.util.TransactionalStringBuilder;
import uk.ac.ucl.excites.sapelli.storage.eximport.xml.XMLRecordsImporter;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.AlertDialog;
import android.app.Dialog;
import android.app.ProgressDialog;
import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.res.AssetManager;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.net.Uri;
import android.os.AsyncTask;
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
import android.widget.Toast;

import com.crashlytics.android.Crashlytics;
import com.ipaulpro.afilechooser.utils.FileUtils;
import com.larvalabs.svgandroid.SVG;
import com.larvalabs.svgandroid.SVGBuilder;
import com.larvalabs.svgandroid.SVGDrawable;

/**
 * @author Julia, Michalis Vitos, mstevens
 * 
 */
public class ProjectManagerActivity extends BaseActivity implements ProjectLoaderCallback, StoreClient, DeviceID.InitialisationCallback
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
			projectStore = app.getProjectStore(this);
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
		app.discardStoreUsage(projectStore, this); // signal that the activity no longer needs the DAO
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
	    	case R.id.copy_db_menuitem :
	    		return copyDBtoSD(item);
			case R.id.zip_files:
			return backupSapelli(this);
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
				"<p><b>" + app.getBuildInfo().getVersionInfo() + "</b></p>" +
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

	public boolean copyDBtoSD(MenuItem item)
	{
		try
		{
			app.backupStores();
		}
		catch(Exception e)
		{
			showErrorDialog(getString(R.string.backupFailDueTo, ExceptionHelpers.getMessageAndCause(e)), false);
		}
		return true;
	}
	
	public boolean backupSapelli(final Context context)
	{
		// Create the items
		final List<String> selectedItems = new ArrayList<String>();
		final List<String> checkboxItems = new ArrayList<String>();
		final List<Boolean> checkedItems = new ArrayList<Boolean>();

		for(int i = 0; i < Folders.values().length; i++)
		{
			final Folders folder = Folders.values()[i];

			switch(folder)
			{
			// Default selected:
			case Dumps:
			case Export:
			case Logs:
				checkboxItems.add(folder.name());
				checkedItems.add(true);
				selectedItems.add(folder.name());
				break;

			// Default unselected:
			case Data:
			case Projects:

				checkboxItems.add(folder.name());
				checkedItems.add(false);
				break;

			// Skip:
			case Downloads:
			case Temp:
			default:
				break;
			}
		}
		
		AlertDialog.Builder builder = new AlertDialog.Builder(this);
		// Set the dialog title
		builder.setTitle("Select folders to export:")
		// Specify the list array, the items to be selected by default (null for none),
		// and the listener through which to receive callbacks when items are selected
				.setMultiChoiceItems(
				// Transform checkboxItems to a CharSequence[]
				checkboxItems.toArray(new CharSequence[checkboxItems.size()]),
				// Transform checkedItems to a boolean[]
				ArrayUtils.toPrimitive(checkedItems.toArray(new Boolean[checkedItems.size()])),
				new DialogInterface.OnMultiChoiceClickListener()
				{
					@Override
					public void onClick(DialogInterface dialog, int which, boolean isChecked)
					{
						if(isChecked)
						{
							// If the user checked the item, add it to the selected items
							selectedItems.add(Folders.values()[which].name());
						}
						else if(selectedItems.contains(Folders.values()[which].name()))
						{
							// Else, if the item is already in the array, remove it
							selectedItems.remove(Folders.values()[which].name());
						}
					}
				})
				// Set the action buttons
				.setPositiveButton(R.string.yes, new DialogInterface.OnClickListener()
				{
					@Override
					public void onClick(DialogInterface dialog, int id)
					{
						// TODO Improve File name
						final String zipfile = fileStorageProvider.getDownloadsFolder()
								+ File.separator + "Sapelli_" + TimeUtils.getTimestampForFileName();

						// Get file paths for the selected items from FileStorageProvider
						List<String> paths = new ArrayList<String>();
						for(String f : selectedItems)
							paths.add(fileStorageProvider.getSapelliFolderPath(Folders.valueOf(f)));

						// Call an AsyncZipper only if there are selected items
						if(!paths.isEmpty())
						{
							AsyncZipper zipper = new AsyncZipper(context, getString(R.string.exporting_data), paths, zipfile);
							zipper.execute();
						}
						else
							Toast.makeText(context, R.string.select_at_least_one_folder_to_export_data, Toast.LENGTH_LONG).show();
					}
				}).setNegativeButton(R.string.no, new DialogInterface.OnClickListener()
				{
					@Override
					public void onClick(DialogInterface dialog, int id)
					{
						// Do nothing
					}
				});

		// Show the dialog
		builder.create().show();

		return false;
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
		
		//Add project
			if(Pattern.matches(Patterns.WEB_URL.toString(), location))
				// Download Sapelli file if location is a URL:
				(new DownloadFileFromURL(location, "Project")).execute(); // the task will also call loadProjectFromFile()
			else if(location.toLowerCase().endsWith("." + XML_FILE_EXTENSION))
				// Warn about bare XML file (no longer supported):
				showErrorDialog(R.string.noBareXMLProjects);
		else
	{
				File localFile = new File(location);
				if(ProjectLoader.HasSapelliFileExtension(localFile))
					loadProjectFromFile(localFile, null);
				else
					showErrorDialog(getString(R.string.unsupportedExtension, FileHelpers.getFileExtension(localFile), StringUtils.join(ProjectLoader.SAPELLI_FILE_EXTENSIONS, ", ")));
			// Use the path where the xml file resides as the basePath (img&snd folders are assumed to be in the same place), no subfolders are created:
			// Show parser warnings if needed:
		}
		}
	}

	/**
	 * Processes a sapelli file (extracting & installing contents, parsing XML, etc.) to construct a Project object,
	 * which, if it passes all checks, is then stored in the ProjectStore. 
	 * 
	 * @param sapelliFile
	 * @param remoteSource remote url project was downloaded from if it was, null otherwise
	 * @return the project object, or null if there was a problem
	 */
	private Project loadProjectFromFile(File sapelliFile, String remoteSource)
	{
		Project project = null;
		ProjectLoader loader = new ProjectLoader(this, fileStorageProvider);
		try
		{
			project = loader.load(sapelliFile);
		}
		catch(Exception e)
		{
			Log.e(TAG, "Could not load Sapelli file", e);
			showErrorDialog(getString(R.string.sapelliFileLoadFailure, (remoteSource == null ? sapelliFile.getAbsolutePath() : remoteSource), ExceptionHelpers.getMessageAndCause(e)), false);
			return null; // !!!
		}

		// Warnings...
		TransactionalStringBuilder bldr = new TransactionalStringBuilder("\n");
		//	Parser warnings:
		List<String> warnings = loader.getParserWarnings(); 
		if(!warnings.isEmpty())
		{ // Show parser warnings:
			bldr.append(getString(R.string.parsingWarnings) + ":");
			for(String warning : warnings)
				bldr.append(" - " + warning);
		}

		List<String> missingFiles = project.getMissingFilesRelativePaths(fileStorageProvider);
		if(!missingFiles.isEmpty())
	{
			bldr.append(getString(R.string.missingFiles) + ":");
			for(String missingFile : missingFiles)
				bldr.append(" - " + missingFile);
		}
		// Check file dependencies
		if(!bldr.isEmpty())
			showWarningDialog(bldr.toString()); // no need to worry about message not fitting the dialog, when necessary it will be scrollable
		
		// Generate documentation
		try
		{
			projectStore.add(project);
		}
		catch(Exception e) // this shouldn't happen, but just in case...
		{
			Log.e(TAG, "Could not store project.", e);
		
			org.apache.commons.io.FileUtils.deleteQuietly(fileStorageProvider.getProjectInstallationFolder(project, false));
		// Store the project object:
			showErrorDialog("Could not store project: " + e.getLocalizedMessage());
			return null; // !!!
		}

		// Update project list:
		populateProjectList();
		selectProjectInList(project); // select the new project

		// TODO Re-enable the service at same point
		// Restart the DataSenderService to start monitoring the new project
		// ServiceChecker.restartActiveDataSender(this);

		// Return project:
		return project;
		}
	
	/**
	 * @param loadedProject
	 * @throws ProjectDuplicateException
	 */
	@Override
	public void checkProject(Project loadedProject) throws ProjectDuplicateException
	{
		projectStore.duplicateCheck(loadedProject);
	}
	
	private void requestEncryptionKey(final Project project)
	{
		// encryptionDialog = new AlertDialog.Builder(this);
		AlertDialog.Builder builder = new AlertDialog.Builder(this);
		builder.setTitle("Project Encryption");
		builder.setMessage("This project requires a password in order to encrypt and transmit the data. Please provide a password:");
		
		// Set an EditText view to get user input
		final EditText input = new EditText(this);
		builder.setView(input);

		builder.setPositiveButton("Ok", new DialogInterface.OnClickListener()
		{
			public void onClick(DialogInterface dialog, int whichButton)
			{
				//String inputStr = input.getText().toString();
				//project.getTransmissionSettings().setPassword(inputStr.isEmpty() ? 	EncryptionSettings.DEFAULT_PASSWORD /*Set the Default Password*/ :
				//																	inputStr);
			}
		});
		encryptionDialog = builder.create();
		encryptionDialog.show();
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
						XMLRecordsImporter importer = new XMLRecordsImporter(app.getCollectorClient());
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

	/**
	 * Background Async Task to download file
	 * 
	 * @author Michalis Vitos, mstevens
	 */
	public class DownloadFileFromURL extends AsyncTask<Void, Integer, Boolean>
	{

		static private final String TEMP_FILE_EXTENSION = "tmp";

		// Variables
		private final long startTime;
		private final ProgressDialog progressDialog;
		private final String downloadUrl;
		private final File downloadFolder;
		private final File downloadFile;

		/**
		 * Downloads the file
		 * 
		 * Note: We do not use Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS); as the download folder because it does not seem
		 * to be writable on the Xcover.
		 * 
		 * @param downloadUrl
		 * @param filename
		 */
		public DownloadFileFromURL(String downloadUrl, String filename)
		{
			startTime = System.currentTimeMillis();
			this.downloadUrl = downloadUrl;
			// Download file in folder /Downloads/timestamp-filename
			downloadFolder = fileStorageProvider.getDownloadsFolder();
			downloadFile = new File(downloadFolder.getAbsolutePath() + File.separator + (startTime / 1000) + '.' + TEMP_FILE_EXTENSION);

			// instantiate it within the onCreate method
			progressDialog = new ProgressDialog(ProjectManagerActivity.this);
			progressDialog.setMessage("Downloading...");
			progressDialog.setIndeterminate(false);
			progressDialog.setMax(100);
			progressDialog.setProgressStyle(ProgressDialog.STYLE_HORIZONTAL);
			progressDialog.setCancelable(true);
		}

		/**
		 * Show Progress Bar Dialog before starting the downloading
		 * */
		@Override
		protected void onPreExecute()
		{
			super.onPreExecute();

			progressDialog.setButton(DialogInterface.BUTTON_POSITIVE, "Cancel...", new DialogInterface.OnClickListener()
			{
				public void onClick(DialogInterface dialog, int which)
				{
					DownloadFileFromURL.this.cancel(true);
					// Delete the downloaded file
					downloadFile.delete();
				}
			});

			progressDialog.show();
		}

		/**
		 * Downloading file in background thread
		 * 
		 * @return
		 * */
		@Override
		protected Boolean doInBackground(Void... voids)
		{
			if(isOnline(ProjectManagerActivity.this))
			{
				int count;
				try
				{
					URL url = new URL(downloadUrl);
					HttpURLConnection connection = (HttpURLConnection) url.openConnection();
					connection.setRequestMethod("GET");
					connection.connect();
					// getting file length
					int fileLength = connection.getContentLength();

					// input stream to read file - with 8k buffer
					InputStream input = new BufferedInputStream(url.openStream(), 8192);
					// Output stream to write file
					OutputStream output = new FileOutputStream(downloadFile);

					byte data[] = new byte[1024];
					long total = 0;
					while((count = input.read(data)) != -1)
					{
						total += count;
						// Publish the progress....
						publishProgress((int) (total * 100 / fileLength));

						// writing data to file
						output.write(data, 0, count);
					}

					// flushing output
					output.flush();

					// closing streams
					output.close();
					input.close();
				}
				catch(Exception e)
				{
					Log.e("Download error: ", e.getMessage(), e);
					return false;
				}
				return true;
			}
			return false;
		}

		/**
		 * Updating progress bar
		 * */
		protected void onProgressUpdate(Integer... progress)
		{
			progressDialog.setProgress(progress[0]);
		}

		/**
		 * After completing background task Dismiss the progress dialog and parse the project
		 * **/
		@Override
		protected void onPostExecute(Boolean downloadFinished)
		{
			// Dismiss the dialog after the file was downloaded
			progressDialog.dismiss();
			if(downloadFinished)
			{
				
				Project project = loadProjectFromFile(downloadFile, downloadUrl); // also handles all exceptions, but returns null if there was one
				// Process the file & add the project to the db & list on the screen
				
				if(project != null)
					downloadFile.renameTo(new File(downloadFolder.getAbsolutePath() + File.separator + project.getName() + "_v" + project.getVersion() + '_' + (startTime / 1000) + ".sapelli"));
				else
					downloadFile.delete();
			}
			else
			{
				showErrorDialog("Download error. Please check if you are connected to the Internet.", false);
				// Delete the downloaded file
				downloadFile.delete();
			}
		}
	}

	/**
	 * Check if the device is connected to Internet
	 * 
	 * @param mContext
	 * @return
	 */
	public static boolean isOnline(Context mContext)
	{
		ConnectivityManager cm = (ConnectivityManager) mContext.getSystemService(Context.CONNECTIVITY_SERVICE);
		NetworkInfo netInfo = cm.getActiveNetworkInfo();
		if(netInfo != null && netInfo.isConnected())
			return true;
		return false;
	}

}