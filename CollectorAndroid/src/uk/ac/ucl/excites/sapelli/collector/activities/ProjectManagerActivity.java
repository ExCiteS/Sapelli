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
import java.util.List;
import java.util.regex.Pattern;

import uk.ac.ucl.excites.sapelli.collector.BuildConfig;
import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.R;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.io.ProjectLoader;
import uk.ac.ucl.excites.sapelli.collector.io.ProjectLoaderClient;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.util.DeviceID;
import uk.ac.ucl.excites.sapelli.collector.util.DuplicateException;
import uk.ac.ucl.excites.sapelli.collector.util.ProjectRunHelpers;
import uk.ac.ucl.excites.sapelli.collector.util.qrcode.IntentIntegrator;
import uk.ac.ucl.excites.sapelli.collector.util.qrcode.IntentResult;
import uk.ac.ucl.excites.sapelli.collector.xml.ProjectParser;
import uk.ac.ucl.excites.sapelli.shared.db.StoreClient;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
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

import com.crashlytics.android.Crashlytics;
import com.ipaulpro.afilechooser.utils.FileUtils;
import com.larvalabs.svgandroid.SVG;
import com.larvalabs.svgandroid.SVGBuilder;
import com.larvalabs.svgandroid.SVGDrawable;

/**
 * @author Julia, Michalis Vitos, mstevens
 * 
 */
public class ProjectManagerActivity extends BaseActivity implements ProjectLoaderClient, StoreClient, DeviceID.InitialisationCallback
{

	// STATICS--------------------------------------------------------
	static private final String TAG = "ProjectManagerActivity";
	
	static private final String XML_FILE_EXTENSION = "xml";

	static private final String DEMO_PROJECT = "demo.excites";

	public static final int RETURN_BROWSE_FOR_PROJECT_LOAD = 1;
	public static final int RETURN_BROWSE_FOR_RECORD_IMPORT = 2;

	// DYNAMICS-------------------------------------------------------
	private ProjectStore projectStore;

	// UI
	private EditText enterURL;
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
		enterURL = (EditText) findViewById(R.id.EnterURL);
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
			@Override
			public boolean onTouch(View v, MotionEvent event)
			{
				projectList.getParent().requestDisallowInterceptTouchEvent(false);
				return false;
			}
		});
		projectList.setOnTouchListener(new View.OnTouchListener()
		{
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
				ProjectLoader loader = new ProjectLoader(this, app.getSapelliFolderPath(), app.getTempFolderPath());
				p = loader.load(this.getAssets().open(DEMO_PROJECT, AssetManager.ACCESS_RANDOM));
				storeProject(p);
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
		Project p = getSelectedProject(false);
		if(p == null)
			return;
		ProjectRunHelpers.removeShortcut(this, p);
		projectStore.delete(p);
		populateProjectList();

		// TODO Re-enable the service at same point
		// Restart the DataSenderService to stop monitoring the deleted project
		// ServiceChecker.restartActiveDataSender(this);
	}

	public void loadFile(View view)
	{
		// Define variables
		String path = enterURL.getText().toString().trim();
		if(path.isEmpty())
		{
			showErrorDialog("Please select an XML or Sapelli file", false);
			return;
		}
		enterURL.setText(""); // clear field
		
		Project project = null;
		try
		{
			// Download Sapelli file if path is a URL
			if(Pattern.matches(Patterns.WEB_URL.toString(), path)) // We don't check the extension to support "smart"/dynamic URLs
			{	// Start async task to download the file:
				(new DownloadFileFromURL(path, "Project")).execute(); // the task will also call processSapelliFile() and checkProject()
				return;
			}
			else if(path.toLowerCase().endsWith(XML_FILE_EXTENSION))
				project = parseXML(new File(path));
			else
			{
				// Extract & parse a local Sapelli file
				for(String extention : ProjectLoader.SAPELLI_FILE_EXTENSIONS)
				{
					if(path.toLowerCase().endsWith(extention))
					{
						project = processSapelliFile(new File(path));
						break;
					}
				}
			}
		}
		catch(Exception e)
		{
			showErrorDialog("Invalid XML or Sapelli file: " + path + "\nError: " + e.getMessage() + (e.getCause() != null ? "\nCause: " + e.getCause().getMessage() : ""), false);
			return;
		}
		
		//Add project
		if(project != null) // check to be sure
			addProject(project);
		else
			showErrorDialog("Please choose a valid XML or Sapelli file", false);
	}

	private Project parseXML(File xmlFile) throws Exception
	{
		try
		{
			// TODO
			// Use the path where the xml file resides as the basePath (img&snd folders are assumed to be in the same place), no subfolders are created:
			ProjectParser parser = new ProjectParser(app.getSapelliFolderPath(), false);
			Project parsedProject = parser.parseProject(xmlFile);
			// Show parser warnings if needed:
			showParserWarnings(parser.getWarnings());
			return parsedProject;
		}
		catch(Exception e)
		{
			Log.e(TAG, "XML file could not be parsed", e);
			throw e;
		}
	}

	private Project processSapelliFile(File sapelliFile) throws Exception
	{
		try
		{
			ProjectLoader loader = new ProjectLoader(this, app.getSapelliFolderPath(), app.getTempFolderPath());
			Project loadedProject = loader.load(sapelliFile);
			// Show parser warnings if needed:
			showParserWarnings(loader.getParserWarnings());
			return loadedProject;
		}
		catch(Exception e)
		{
			Log.e(TAG, "Could not load Sapelli file", e);
			throw e;
		}
	}

	private void showParserWarnings(List<String> warnings)
	{
		if(!warnings.isEmpty())
		{ // Show parser warnings:
			String msg = "Parsing issues:\n";
			for(String warning : warnings)
				msg += warning + "\n";
			showWarningDialog(msg);
		}
	}

	private void addProject(final Project project)
	{
		// Check file dependencies
		List<String> invalidFiles = project.checkForInvalidFiles();
		if(!invalidFiles.isEmpty())
			showWarningDialog("The following files could not be found or read in the project path (" + project.getProjectFolderPath() + "): " + StringUtils.join(invalidFiles, ", "));
		
		// Store the project object:
		storeProject(project);

		// Update project list:
		populateProjectList();
		selectProjectInList(project); // select the new project

		// TODO Re-enable the service at same point
		// Restart the DataSenderService to start monitoring the new project
		// ServiceChecker.restartActiveDataSender(this);
	}

	private void storeProject(Project p)
	{
		try
		{
			projectStore.store(p);
		}
		catch(DuplicateException de)
		{
			showErrorDialog(de.getLocalizedMessage(), false);
			return;
		}
		catch(Exception e) // any other exception
		{
			Log.e(TAG, "Could not store project.", e);
			showErrorDialog("Could not store project: " + e.getLocalizedMessage(), false);
			return;
		}
	}
	
	@Override
	public boolean isDuplicateProject(Project loadProject)
	{
		// TODO Auto-generated method stub
		return false;
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
			ProjectRunHelpers.createShortcut(this, selectedProject);
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
					enterURL.setText(path);
					// Move the cursor to the end
					enterURL.setSelection(path.length());
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
						showParserWarnings(importer.getWarnings());

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
					enterURL.setText(fileUrl);
					// Move the cursor to the end
					enterURL.setSelection(fileUrl.length());
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
			downloadFolder = new File(app.getDownloadFolderPath());
			FileHelpers.createFolder(downloadFolder);
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
				Project project = null;
				
				// Process the file & add the project to the db & list on the screen
				try
				{
					project = processSapelliFile(downloadFile);
					addProject(project); // will show error if project is null
				}
				catch(Exception e)
				{
					showErrorDialog("Invalid Sapelli file: " + downloadUrl + "\nError: " + e.getMessage()
							+ (e.getCause() != null ? "\nCause: " + e.getCause().getMessage() : ""), false);
					return;
				}
				
				// Handle temp file:
				if(project != null)
					downloadFile.renameTo(new File(downloadFolder.getAbsolutePath() + File.separator + project.getName() + "_v" + project.getVersion() + '_'
							+ (startTime / 1000) + ".sapelli"));
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