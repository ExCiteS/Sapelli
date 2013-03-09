package uk.ac.ucl.excites.collector;

import group.pals.android.lib.ui.filechooser.FileChooserActivity;
import group.pals.android.lib.ui.filechooser.io.localfile.LocalFile;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.net.URLConnection;
import java.util.List;
import java.util.regex.Pattern;

import uk.ac.ucl.excites.collector.project.db.DataAccess;
import uk.ac.ucl.excites.collector.project.io.ExCiteSFileLoader;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.util.DuplicateException;
import uk.ac.ucl.excites.collector.project.xml.ProjectParser;
import uk.ac.ucl.excites.collector.ui.BaseActivity;
import uk.ac.ucl.excites.collector.util.SDCard;
import android.app.Activity;
import android.app.AlertDialog;
import android.app.ProgressDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.Environment;
import android.os.Parcelable;
import android.util.Log;
import android.util.Patterns;
import android.view.MotionEvent;
import android.view.View;
import android.view.Window;
import android.view.WindowManager;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;

/**
 * @author Julia, Michalis Vitos, mstevens
 * 
 */
public class ProjectPickerActivity extends BaseActivity
{

	//STATIC---------------------------------------------------------
	static private final String TAG = "ProjectPickerActivity";

	static private final String XML_FILE_EXTENSION = "xml";
	static private final String EXCITES_FOLDER = "ExCiteS" + File.separatorChar;

	public static final int RETURN_BROWSE = 1;
	
	//DYNAMIC--------------------------------------------------------
	private String databasePath;
	private DataAccess dao;
	
	//UI
	private EditText enterURL;
	private ListView projectList;
	private Button runBtn;
	private Button removeBtn;
	
	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		// Remove title
		requestWindowFeature(Window.FEATURE_NO_TITLE);
		// Hide soft keyboard on create
		getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_HIDDEN);
		setContentView(R.layout.activity_projectpicker);

		// Database instance (path may be changed)
		// Path is on Internal Storage
		databasePath = this.getFilesDir().getAbsolutePath();
		// Log.d("ExCiteS_Debug", "Internal Storage path: " + dbPATH);
		dao = DataAccess.getInstance(databasePath);

		// Get View Elements
		enterURL = (EditText) findViewById(R.id.EnterURL);
		projectList = (ListView) findViewById(R.id.ProjectsList);
		runBtn = (Button) findViewById(R.id.RunButton);
		removeBtn = (Button) findViewById(R.id.RemoveButton);
		
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
		
		// Check if there is an SD Card
		if(!SDCard.isExternalStorageWritable())
		{	// Inform the user and close the application
			errorDialog("ExCiteS needs an SD card in order to function. Please insert one and restart the application.", true).show();
		}
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
		projectList.setAdapter(new ArrayAdapter<Project>(this, android.R.layout.simple_list_item_single_choice, android.R.id.text1, dao.retrieveProjects()));
		if(!projectList.getAdapter().isEmpty())
		{
			runBtn.setEnabled(true);
			removeBtn.setEnabled(true);
			projectList.setItemChecked(0, true); //check first project in the list
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
		Intent i = new Intent(this, CollectorActivity.class);
		i.putExtra(CollectorActivity.PARAMETER_PROJECT_NAME, p.getName());
		i.putExtra(CollectorActivity.PARAMETER_PROJECT_VERSION, p.getVersion());
		i.putExtra(CollectorActivity.PARAMETER_DB_FOLDER_PATH, databasePath);
		startActivity(i);
	}

	private void removeProject()
	{
		Project p = getSelectedProject();
		if(p == null)
			return;
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
				
		Project project = null;
		
		// Download ExCiteS file if path is a URL
		if(Pattern.matches(Patterns.WEB_URL.toString(), path) /*&& path.toLowerCase().endsWith(ExCiteSFileLoader.EXCITES_FILE_EXTENSION)*/) //extension check commented out to support "smart" URLs
		{
			//start async task to download the file, the task will also call processExcitesFile() and checkProject()
			(new DownloadFileFromURL(path, "Project")).execute();
			return;
		}
		// Extract & parse a local ExCiteS file
		else if(path.toLowerCase().endsWith(ExCiteSFileLoader.EXCITES_FILE_EXTENSION))
		{
			project = processExcitesFile(new File(path));
		}
		// Parse a local XML file
		else if(path.toLowerCase().endsWith(XML_FILE_EXTENSION))
		{
			project = parseXML(new File(path));
		}
		//Add the project to the db & the list on the screen:
		addProject(project, path); //null check (with appropriate error) happens in addProject()
	}
	
	private Project parseXML(File xmlFile)
	{
		try
		{
			// Use the path where the xml file currently is as the basePath (img and snd folders are assumed to be in the same place), no subfolders are created:
			ProjectParser parser = new ProjectParser(xmlFile.getParentFile().getAbsolutePath(), false);
			return parser.parseProject(xmlFile);
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
				ExCiteSFileLoader loader = new ExCiteSFileLoader(Environment.getExternalStorageDirectory().getAbsolutePath() + File.separatorChar + EXCITES_FOLDER);
				return loader.load(excitesFile);
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
	
	private void addProject(Project project, String sourcePathOrURL)
	{
		// Check if we have a project object:
		if(project == null)
		{
			errorDialog("Invalid xml or excites file: " + sourcePathOrURL, false).show();
			return;
		}
		// Store the project object:
		try
		{
			dao.store(project);
		}
		catch(DuplicateException de)
		{
			errorDialog(de.getLocalizedMessage(), false).show();
			return;
		}
		catch(Exception e) //any other exception
		{
			Log.e(TAG, "Could not store project.", e);
			errorDialog("Could not store project: " + e.getLocalizedMessage(), false).show();
			return;
		}
		// Update project list:
		populateProjectList();
		selectProjectInList(project); //select the new project
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data)
	{
		super.onActivityResult(requestCode, resultCode, data);
		if(resultCode == Activity.RESULT_OK)
		{
			switch(requestCode)
			{
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
			}
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
		{
			errorDialog("Please select a project", false).show();
		}
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
		dao.closeDB();
	}

	@Override
	protected void onResume()
	{
		// open database
		super.onResume();
		dao.openDB();
		// Update project list:
		populateProjectList();
	}

	/**
	 * Background Async Task to download file
	 * 
	 * @author Michalis Vitos, mstevens
	 * */
	public class DownloadFileFromURL extends AsyncTask<Void, Integer, Void>
	{
		
		static private final String TEMP_FILE_EXTENSION = "tmp";
		
		// Variables
		private long startTime;
		private String downloadUrl;
		private File downloadFolder;
		private File downloadFile;
		private ProgressDialog progressDialog;
		
		public DownloadFileFromURL(String downloadUrl, String tempFilePrefix)
		{
			this.startTime = System.currentTimeMillis();
			
			this.downloadUrl = downloadUrl;
			// Download file in folder /Download/timestamp-filename
			this.downloadFolder = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS);
			this.downloadFile = new File(downloadFolder.getAbsolutePath() + File.separator + tempFilePrefix + '_' + (startTime / 1000) + '.' + TEMP_FILE_EXTENSION);
			
			// Set-up the progress dialog
			progressDialog = new ProgressDialog(ProjectPickerActivity.this);
			progressDialog.setMessage("Downloading...");
			progressDialog.setIndeterminate(false);
			progressDialog.setMax(100);
			progressDialog.setProgressStyle(ProgressDialog.STYLE_HORIZONTAL);
			progressDialog.setCancelable(false);
		}

		/**
		 * Show Progress Bar Dialog before starting the downloading
		 * */
		@Override
		protected void onPreExecute()
		{
			super.onPreExecute();
			progressDialog.show();
		}

		/**
		 * Downloading file in background thread
		 * 
		 * @return
		 * */
		@Override
		protected Void doInBackground(Void... voids)
		{
			int count;
			try
			{
				URL url = new URL(downloadUrl);
				URLConnection conection = url.openConnection();
				conection.connect();
				// getting file length
				int fileLength = conection.getContentLength();

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
			}
			return null;
		}

		/**
		 * Updating progress bar
		 */
		protected void onProgressUpdate(Integer... progress)
		{
			progressDialog.setProgress(progress[0]);
		}

		/**
		 * After completing background task Dismiss the progress dialog and parse the project
		 * **/
		@Override
		protected void onPostExecute(Void voids)
		{
			// Dismiss the dialog after the file was downloaded
			progressDialog.dismiss();

			// Process the file & add the project to the db & list on the screen
			Project project = processExcitesFile(downloadFile);
			addProject(project, downloadUrl); //will show error if project is null
			
			// Handle temp file:
			if(project != null)
			{	//Rename temp file:
				downloadFile.renameTo(new File(downloadFolder.getAbsolutePath() + File.separator + project.getName() + "_v" + project.getVersion() + '_' + (startTime / 1000) + ".excites"));
			}
			else
			{	//Delete temp file:
				downloadFile.delete();
			}
		}
	}
	
}
