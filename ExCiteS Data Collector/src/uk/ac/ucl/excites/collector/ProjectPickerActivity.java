package uk.ac.ucl.excites.collector;

import group.pals.android.lib.ui.filechooser.FileChooserActivity;
import group.pals.android.lib.ui.filechooser.io.localfile.LocalFile;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.List;
import java.util.regex.Pattern;

import uk.ac.ucl.excites.collector.project.db.DataAccess;
import uk.ac.ucl.excites.collector.project.io.ExCiteSFileLoader;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.util.DuplicateException;
import uk.ac.ucl.excites.collector.project.xml.ProjectParser;
import uk.ac.ucl.excites.collector.ui.BaseActivity;
import uk.ac.ucl.excites.collector.util.SDCard;
import uk.ac.ucl.excites.sender.DataSenderPreferences;
import uk.ac.ucl.excites.transmission.Settings;
import uk.ac.ucl.excites.util.FileHelpers;
import android.app.Activity;
import android.app.AlertDialog;
import android.app.Dialog;
import android.app.ProgressDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.Environment;
import android.os.Parcelable;
import android.util.Log;
import android.util.Patterns;
import android.view.Menu;
import android.view.MenuItem;
import android.view.MotionEvent;
import android.view.View;
import android.view.WindowManager;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;

import com.google.zxing.integration.android.IntentIntegrator;
import com.google.zxing.integration.android.IntentResult;

/**
 * @author Julia, Michalis Vitos, mstevens
 * 
 */
public class ProjectPickerActivity extends BaseActivity implements MenuItem.OnMenuItemClickListener
{

	// STATICS--------------------------------------------------------
	static private final String TAG = "ProjectPickerActivity";

	static private final String XML_FILE_EXTENSION = "xml";
	static private final String EXCITES_FOLDER = "ExCiteS" + File.separatorChar;
	static private final String DOWNLOADS_FOLDER = "Downloads" + File.separatorChar;
	static private final String DB4O_DUMP_NAME = "DatabaseDump_";
	static private final String DB4O_DUMP_EXTENSION = "db4o";
	
	// SHORTCUT ACTIONS
	private static final String DEFAULT_INSTALL_SHORTCUT_ACTION = "com.android.launcher.action.INSTALL_SHORTCUT";
	private static final String CUSTOM_INSTALL_SHORTCUT_ACTION = "uk.ac.ucl.excites.launcher.INSTALL_SHORTCUT";
	private static final String DEFAULT_UNISTALL_SHORTCUT_ACTION = "com.android.launcher.action.UNINSTALL_SHORTCUT";
	private static final String CUSTOM_UNISTALL_SHORTCUT_ACTION = "uk.ac.ucl.excites.launcher.UNINSTALL_SHORTCUT";
	public static final String SHORTCUT_PROJECT_NAME = "Shortcut_Project_Name";
	public static final String SHORTCUT_PROJECT_VERSION = "Shortcut_Project_Version";
	public static final String SHORTCUT_PROJECT_ICON = "Shortcut_Project_Icon";
	public static final String SHORTCUT_PROJECT_DB = "Shortcut_Project_Database";

	public static final int RETURN_BROWSE = 1;

	// DYNAMICS-------------------------------------------------------
	private String databasePath;
	private String excitesFolderPath;
	private DataAccess dao;

	// UI
	private EditText enterURL;
	private ListView projectList;
	private Button runBtn;
	private Button removeBtn;
	private Button createShortcutBtn;
	private Button removeShortcutBtn;
	private MenuItem senderSettingsItem;
	private MenuItem copyDBItem;
	private Dialog encryptionDialog;

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		
		// Check if there is an SD Card
		if(!SDCard.isExternalStorageWritable())
		{ // Inform the user and close the application
			errorDialog("ExCiteS needs an SD card in order to function. Please insert one and restart the application.", true).show();
			return;
		}

		// Paths...
		// Database path is on Internal Storage
		databasePath = getFilesDir().getAbsolutePath();
		// ExCiteS folder
		excitesFolderPath = Environment.getExternalStorageDirectory().getAbsolutePath() + File.separatorChar + EXCITES_FOLDER;

		// DataAccess instance:
		dao = DataAccess.getInstance(databasePath);

		// Set-up UI...
		setTitle("ExCiteS Project Picker");
		// Hide soft keyboard on create
		getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_HIDDEN);
		setContentView(R.layout.activity_projectpicker);
		// Get View Elements
		enterURL = (EditText) findViewById(R.id.EnterURL);
		projectList = (ListView) findViewById(R.id.ProjectsList);
		runBtn = (Button) findViewById(R.id.RunProjectButton);
		removeBtn = (Button) findViewById(R.id.RemoveProjectButton);
		createShortcutBtn = (Button) findViewById(R.id.CreateShortcutButton);
		removeShortcutBtn = (Button) findViewById(R.id.RemoveShortcutButton);
		// get scrolling right
		findViewById(R.id.scrollView).setOnTouchListener(new View.OnTouchListener()
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
			{
				// Disallow the touch request for parent scroll on touch of child view
				v.getParent().requestDisallowInterceptTouchEvent(true);
				return false;
			}
		});
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu)
	{
		getMenuInflater().inflate(R.menu.projectpicker, menu);

		// Set click listeners (the android:onClick attribute in the XML only works on Android >= v3.0)
		senderSettingsItem = menu.findItem(R.id.sender_settings_menuitem);
		if(senderSettingsItem != null)
			senderSettingsItem.setOnMenuItemClickListener(this);
		copyDBItem = menu.findItem(R.id.copy_db_menuitem);
		if(copyDBItem != null)
			copyDBItem.setOnMenuItemClickListener(this);
		return true;
	}
	
	@Override
	public boolean onMenuItemClick(MenuItem item)
	{
		if(item == senderSettingsItem)
			return openSenderSettings(item);
		else if(item == copyDBItem)
			return copyDBtoSD(item);
		return false;
	}

	public boolean openSenderSettings(MenuItem item)
	{
		startActivity(new Intent(getBaseContext(), DataSenderPreferences.class));
		return true;
	}
	
	public boolean copyDBtoSD(MenuItem item)
	{
		dao.closeDB();
		dao.copyDB(excitesFolderPath + DB4O_DUMP_NAME + System.currentTimeMillis() + "." + DB4O_DUMP_EXTENSION);
		dao.openDB();
		return true;
	}

	public void browse(View view)
	{
		Intent intent = new Intent(getBaseContext(), FileChooserActivity.class);
		// Start from "/sdcard"
		intent.putExtra(FileChooserActivity._Rootpath, (Parcelable) new LocalFile(Environment.getExternalStorageDirectory().getPath()));
		// set file filter for .xml or .excites
		intent.putExtra(FileChooserActivity._RegexFilenameFilter, "^.*\\.(" + XML_FILE_EXTENSION + "|" + ExCiteSFileLoader.EXCITES_FILE_EXTENSION + ")$");
		startActivityForResult(intent, RETURN_BROWSE);
	}

	/**
	 * Retrieve all parsed projects from db and populate list
	 */
	public void populateProjectList()
	{
		projectList.setAdapter(new ArrayAdapter<Project>(this, R.layout.project_list, android.R.id.text1, dao.retrieveProjects()));
		if(!projectList.getAdapter().isEmpty())
		{
			runBtn.setEnabled(true);
			removeBtn.setEnabled(true);
			createShortcutBtn.setEnabled(true);
			removeShortcutBtn.setEnabled(true);
			projectList.setItemChecked(0, true); // check first project in the list
		}
		else
		{
			runBtn.setEnabled(false);
			removeBtn.setEnabled(false);
			createShortcutBtn.setEnabled(false);
			removeShortcutBtn.setEnabled(false);
		}
	}

	@SuppressWarnings("unchecked")
	protected void selectProjectInList(Project project)
	{
		projectList.setItemChecked(((ArrayAdapter<Project>) projectList.getAdapter()).getPosition(project), true);
	}

	@SuppressWarnings("unchecked")
	protected Project getSelectedProject()
	{
		if(projectList.getCheckedItemPosition() == -1)
			return null;
		return ((ArrayAdapter<Project>) projectList.getAdapter()).getItem(projectList.getCheckedItemPosition());
	}

	public void runProject(View view)
	{
		Project p = getSelectedProject();
		if(p == null)
		{
			errorDialog("Please select a project", false).show();
			return;
		}
		runProjectActivity(p.getName(), p.getVersion());
	}

	public void runProjectActivity(String projectName, String projectVersion)
	{
		Intent i = new Intent(this, CollectorActivity.class);
		i.putExtra(CollectorActivity.PARAMETER_PROJECT_NAME, projectName);
		i.putExtra(CollectorActivity.PARAMETER_PROJECT_VERSION, projectVersion);
		i.putExtra(CollectorActivity.PARAMETER_DB_FOLDER_PATH, databasePath);
		startActivity(i);
	}

	private void removeProject()
	{
		Project p = getSelectedProject();
		if(p == null)
			return;
		removeShortcutFor(p);
		dao.deleteProject(p);
		populateProjectList();
	}

	public void loadFile(View view)
	{
		// Define variables
		String path = enterURL.getText().toString().trim();
		if(path.isEmpty())
		{
			errorDialog("Please select an XML or ExCiteS file", false).show();
			return;
		}
		enterURL.setText(""); // clear field
		Project project = null;

		// Download ExCiteS file if path is a URL
		if(Pattern.matches(Patterns.WEB_URL.toString(), path) /* && path.toLowerCase().endsWith(ExCiteSFileLoader.EXCITES_FILE_EXTENSION) */)
		{ // Extension check above is commented out to support "smart"/dynamic URLs
			// Start async task to download the file:
			(new DownloadFileFromURL(path, "Project")).execute(); // the task will also call processExcitesFile() and checkProject()
			return;
		}
		// Extract & parse a local ExCiteS file
		else if(path.toLowerCase().endsWith(ExCiteSFileLoader.EXCITES_FILE_EXTENSION))
			project = processExcitesFile(new File(path));
		else if(path.toLowerCase().endsWith(XML_FILE_EXTENSION))
			project = parseXML(new File(path));
		// Add the project to the db & the list on the screen:
		addProject(project, path); // null check (with appropriate error) happens in addProject()
	}

	private Project parseXML(File xmlFile)
	{
		try
		{
			// Use the path where the xml file currently is as the basePath (img and snd folders are assumed to be in the same place), no subfolders are
			// created:
			ProjectParser parser = new ProjectParser(xmlFile.getParentFile().getAbsolutePath(), false);
			Project parsedProject = parser.parseProject(xmlFile);
			//Show parser warnings if needed:
			showParserWarnings(parser.getWarnings());
			return parsedProject;
		}
		catch(Exception e)
		{
			Log.e(TAG, "XML file could not be parsed", e);
			return null;
		}
	}

	private Project processExcitesFile(File excitesFile)
	{
		try
		{
			// Check if there is an SD Card
			if(SDCard.isExternalStorageWritable())
			{
				// Use /mnt/sdcard/ExCiteS/ as the basePath:
				ExCiteSFileLoader loader = new ExCiteSFileLoader(excitesFolderPath);
				Project loadedProject = loader.load(excitesFile);
				//Show parser warnings if needed:
				showParserWarnings(loader.getParserWarnings());
				return loadedProject;
			}
			else
			{
				// Inform the user and close the application
				errorDialog("ExCiteS needs an SD card in order to function. Please insert one and restart the application.", true).show();
				return null;
			}
		}
		catch(Exception e)
		{
			Log.e(TAG, "Could not load excites file", e);
			return null;
		}
	}
	
	private void showParserWarnings(List<String> warnings)
	{
		if(!warnings.isEmpty())
		{	//Show parser warnings:
			String msg = "Parsing issues:\n";
			for(String warning : warnings)
				msg += warning + "\n";
			warningDialog(msg).show();
		}
	}

	private void addProject(final Project project, String sourcePathOrURL)
	{
		// Check if we have a project object:
		if(project == null)
		{
			errorDialog("Invalid xml or excites file: " + sourcePathOrURL, false).show();
			return;
		}

		// Encryption Check
		if(project.getTransmissionSettings().isEncrypt())
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
					if(input.getText().toString().equals(""))
					{
						// Set the Default Password
						project.getTransmissionSettings().setPassword(Settings.DEFAULT_PASSWORD);
						// Store the project object:
						storeProject(project);
					}
					else
					{
						// Set the Password
						project.getTransmissionSettings().setPassword(input.getText().toString());
						// Store the project object:
						storeProject(project);
					}
				}
			});

			encryptionDialog = builder.create();
			encryptionDialog.show();
		}
		else
			storeProject(project);
	}

	private void storeProject(Project p)
	{
		try
		{
			dao.store(p);
		}
		catch(DuplicateException de)
		{
			errorDialog(de.getLocalizedMessage(), false).show();
			return;
		}
		catch(Exception e) // any other exception
		{
			Log.e(TAG, "Could not store project.", e);
			errorDialog("Could not store project: " + e.getLocalizedMessage(), false).show();
			return;
		}

		// Update project list:
		populateProjectList();
		selectProjectInList(p); // select the new project
	}

	public void scanQR(View view)
	{
		// Start the Intent to Scan a QR code
		IntentIntegrator integrator = new IntentIntegrator(this);
		integrator.initiateScan();
	}

	/**
	 * Create a shortcut
	 * 
	 * @param view
	 */
	public void createShortcut(View view)
	{
		// Check if the user has selected a project from the list
		if(projectList.getCheckedItemPosition() == -1)
		{
			errorDialog("Please select a project", false).show();
			return;
		}

		// Get the selected project
		Project selectedProject = getSelectedProject();

		// Set the shortcut intent
		Intent projectIntent = new Intent(getApplicationContext(), CollectorActivity.class);
		projectIntent.putExtra(SHORTCUT_PROJECT_NAME, selectedProject.getName());
		projectIntent.putExtra(SHORTCUT_PROJECT_VERSION, selectedProject.getVersion());
		projectIntent.putExtra(SHORTCUT_PROJECT_DB, databasePath);
		projectIntent.setAction(Intent.ACTION_MAIN);

		// Set up the icon
		Drawable iconResource = null;
		String shortcutFullPath = null;
		String shortcutLogicalPath = selectedProject.getForms().get(0).getShortcutImageLogicalPath();

		if(shortcutLogicalPath == null || shortcutLogicalPath.isEmpty())
			iconResource = getResources().getDrawable(R.drawable.excites_icon);
		else
		{
			iconResource = Drawable.createFromPath(selectedProject.getImageFolderPath() + shortcutLogicalPath);
			shortcutFullPath = selectedProject.getImageFolderPath() + shortcutLogicalPath;
		}

		BitmapDrawable bitmapDrawable = (BitmapDrawable) iconResource;

		// ================================================================================
		// Create a shortcut to the standard Android Home Launcher
		// ================================================================================
		Intent shortcutIntent = new Intent();
		shortcutIntent.setAction(DEFAULT_INSTALL_SHORTCUT_ACTION);
		shortcutIntent.putExtra(Intent.EXTRA_SHORTCUT_INTENT, projectIntent);
		shortcutIntent.putExtra(Intent.EXTRA_SHORTCUT_NAME, getShortcutName(selectedProject));
		shortcutIntent.putExtra(Intent.EXTRA_SHORTCUT_ICON, bitmapDrawable.getBitmap());
		// Do not allow duplicate shortcuts
		shortcutIntent.putExtra("duplicate", false);
		sendBroadcast(shortcutIntent);

		// ================================================================================
		// Create an Intent to work with the ExCiteS Launcher
		// ================================================================================
		Intent launcherIntent = new Intent();
		launcherIntent.setAction(CUSTOM_INSTALL_SHORTCUT_ACTION);
		launcherIntent.putExtra(Intent.EXTRA_SHORTCUT_INTENT, projectIntent);
		launcherIntent.putExtra(Intent.EXTRA_SHORTCUT_NAME, getShortcutName(selectedProject));
		launcherIntent.putExtra(Intent.EXTRA_SHORTCUT_ICON, bitmapDrawable.getBitmap());
		launcherIntent.putExtra(SHORTCUT_PROJECT_ICON, shortcutFullPath);
		sendBroadcast(launcherIntent);
	}

	/**
	 * Remove a shortcut
	 * 
	 * @param view
	 */
	public void removeShortcut(View view)
	{
		// Check if the user has selected a project from the list
		if(projectList.getCheckedItemPosition() == -1)
		{
			errorDialog("Please select a project", false).show();
			return;
		}
		removeShortcutFor(getSelectedProject());
	}

	private void removeShortcutFor(Project project)
	{
		// Deleting shortcut
		Intent projectIntent = new Intent(getApplicationContext(), CollectorActivity.class);
		projectIntent.putExtra(SHORTCUT_PROJECT_NAME, project.getName());
		projectIntent.putExtra(SHORTCUT_PROJECT_VERSION, project.getVersion());
		projectIntent.putExtra(SHORTCUT_PROJECT_DB, databasePath);
		projectIntent.setAction(Intent.ACTION_MAIN);

		// ================================================================================
		// Remove a shortcut from the standard Android Home Launcher
		// ================================================================================
		Intent shortcutIntent = new Intent();
		shortcutIntent.setAction(DEFAULT_UNISTALL_SHORTCUT_ACTION);
		shortcutIntent.putExtra(Intent.EXTRA_SHORTCUT_INTENT, projectIntent);
		shortcutIntent.putExtra(Intent.EXTRA_SHORTCUT_NAME, getShortcutName(project));
		sendBroadcast(shortcutIntent);

		// ================================================================================
		// Remove a shortcut from the ExCiteS Launcher
		// ================================================================================
		Intent launcherIntent = new Intent();
		launcherIntent.setAction(CUSTOM_UNISTALL_SHORTCUT_ACTION);
		launcherIntent.putExtra(Intent.EXTRA_SHORTCUT_INTENT, projectIntent);
		launcherIntent.putExtra(Intent.EXTRA_SHORTCUT_NAME, getShortcutName(project));
		sendBroadcast(launcherIntent);
	}

	/**
	 * Return a name to be used for the creation / removal of shortcuts
	 * 
	 * @param project
	 * @return
	 */
	public static String getShortcutName(Project project)
	{
		return project.getName() + " v" + project.getVersion();
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data)
	{
		super.onActivityResult(requestCode, resultCode, data);
		if(resultCode == Activity.RESULT_OK)
			switch(requestCode)
			{
			// File browse dialog:
			case RETURN_BROWSE:
				// Get the result file path
				// A list of files will always return, if selection mode is single, the list contains one file
				@SuppressWarnings("unchecked")
				List<LocalFile> files = (List<LocalFile>) data.getSerializableExtra(FileChooserActivity._Results);
				for(File f : files)
				{
					String fileSource = f.getAbsoluteFile().toString();
					enterURL.setText(fileSource);
					// Move the cursor to the end
					enterURL.setSelection(fileSource.length());
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
		if(projectList.getCheckedItemPosition() == -1)
			errorDialog("Please select a project", false).show();
		else
		{
			AlertDialog removeDialogBox = new AlertDialog.Builder(this).setMessage("Are you sure that you want to remove the project?")
					.setPositiveButton("Yes", new DialogInterface.OnClickListener()
					{
						public void onClick(DialogInterface dialog, int whichButton)
						{
							removeProject();
						}
					}).setNegativeButton("Cancel", new DialogInterface.OnClickListener()
					{
						public void onClick(DialogInterface dialog, int whichButton)
						{
						}
					}).create();
			removeDialogBox.show();
		}
	}

	@Override
	protected void onPause()
	{
		// close database
		super.onPause();
		if(dao != null)
			dao.closeDB();

		if(encryptionDialog != null)
			encryptionDialog.dismiss();
	}

	@Override
	protected void onResume()
	{
		// open database
		super.onResume();
		if(dao != null)
		{
			dao.openDB();
			// Update project list:
			populateProjectList();
		}
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
			// Download file in folder /Download/timestamp-filename
			downloadFolder = new File(excitesFolderPath + DOWNLOADS_FOLDER);
			FileHelpers.createFolder(downloadFolder);
			downloadFile = new File(downloadFolder.getAbsolutePath() + File.separator + (startTime / 1000) + '.' + TEMP_FILE_EXTENSION);

			// instantiate it within the onCreate method
			progressDialog = new ProgressDialog(ProjectPickerActivity.this);
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
			if(isOnline(ProjectPickerActivity.this))
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
				// Process the file & add the project to the db & list on the screen
				Project project = processExcitesFile(downloadFile);
				addProject(project, downloadUrl); // will show error if project is null

				// Handle temp file:
				if(project != null)
					downloadFile.renameTo(new File(downloadFolder.getAbsolutePath() + File.separator + project.getName() + "_v" + project.getVersion() + '_'
							+ (startTime / 1000) + ".excites"));
				else
					downloadFile.delete();
			}
			else
			{
				errorDialog("Download error. Please check if you are connected to the Internet.", false).show();
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