package uk.ac.ucl.excites.collector;

import java.io.File;
import java.util.List;
import java.util.regex.Pattern;

import uk.ac.ucl.excites.collector.project.db.DataAccess;
import uk.ac.ucl.excites.collector.project.io.ExCiteSFileLoader;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.util.DuplicateException;
import uk.ac.ucl.excites.collector.project.xml.ProjectParser;
import uk.ac.ucl.excites.collector.ui.filedialog.FileDialog;
import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.os.Environment;
import android.util.Log;
import android.util.Patterns;
import android.view.MotionEvent;
import android.view.View;
import android.view.Window;
import android.view.WindowManager;
import android.widget.ArrayAdapter;
import android.widget.EditText;
import android.widget.ListView;

/**
 * @author Julia, Michalis Vitos, mstevens
 * 
 */
public class ProjectPickerActivity extends Activity
{

	static private final String TAG = "ProjectPickerActivity";
	
	static private final String XML_FILE_EXTENSION = "xml";
	static private final String EXCITES_FOLDER = "ExCiteS" + File.separatorChar; 

	private String dbPATH;

	// Define some variables
	public static final int SETTINGS_REQUEST_IMPORT = 1;
	private EditText enterURL;
	private ListView projectList;
	DataAccess dao;
	List<Project> parsedProjects;

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
		dbPATH = this.getFilesDir().getAbsolutePath();
		Log.d("ExCiteS_Debug", "Internal storage path: " + dbPATH);
		dao = DataAccess.getInstance(dbPATH);

/*		// TODO Copy function
		String dstFilePath =  Environment.getExternalStorageDirectory().getPath() + File.separator + "0000" + File.separator + "ExCiteS_copy.db4o";
		Log.e("ExCiteS_Debug", "Copy path:" + dstFilePath);
		dao.copyDBtoSD(dstFilePath);
*/
		// Get View Elements
		enterURL = (EditText) findViewById(R.id.EnterURL);
		projectList = (ListView) findViewById(R.id.ProjectsList);

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

		// display parsed projects
		populateProjectList();
	}

	public void browse(View view)
	{
		Intent mIntent = new Intent(getBaseContext(), FileDialog.class);
		// Start from "/sdcard"
		mIntent.putExtra(FileDialog.START_PATH, Environment.getExternalStorageDirectory().getPath());

		// can user select directories or not
		mIntent.putExtra(FileDialog.CAN_SELECT_DIR, true);

		// set file filter
		mIntent.putExtra(FileDialog.FORMAT_FILTER, new String[] { XML_FILE_EXTENSION, ExCiteSFileLoader.EXCITES_FILE_EXTENSION });
		startActivityForResult(mIntent, SETTINGS_REQUEST_IMPORT);
	}

	public void runProject(View view)
	{
		if(projectList.getCheckedItemPosition() == -1)
		{
			AlertDialog NoSelection = errorDialog("Please select a project");
			NoSelection.show();
		}
		Project selectedProject = parsedProjects.get(projectList.getCheckedItemPosition());
		Intent i = new Intent(this, CollectorActivity.class);
		i.putExtra(CollectorActivity.PARAMETER_PROJECT_NAME, selectedProject.getName());
		i.putExtra(CollectorActivity.PARAMETER_PROJECT_VERSION, selectedProject.getVersion());
		i.putExtra(CollectorActivity.PARAMETER_DB_FOLDER_PATH, dbPATH);
		startActivity(i);
	}

	public void removeProject()
	{
		dao.deleteProject(parsedProjects.get(projectList.getCheckedItemPosition()));
		populateProjectList();
	}

	public void loadFile(View view)
	{
		if(enterURL.getText().length() == 0)
		{
			errorDialog("Please select an XML or ExCiteS file").show();
			return;
		}
		String path = enterURL.getText().toString();
		enterURL.setText("");
				
		//Download ExCiteS file if necessary
		if(Pattern.matches(Patterns.WEB_URL.toString(), path) && path.toLowerCase().endsWith(ExCiteSFileLoader.EXCITES_FILE_EXTENSION))
		{
			//Download the file...
			String pathToDownloadedFile = null;
			//TODO
			path = pathToDownloadedFile;
		}
		
		//Try to load and/or parse...
		Project project = null;
		if(path.toLowerCase().endsWith(XML_FILE_EXTENSION))
		{
			//Parse XML file...
			try
			{
				File xmlFile = new File(path);
				//Use the path where the xml file currently is as the basePath (img and snd folders are assumed to be in the same place):
				ProjectParser parser = new ProjectParser(xmlFile.getParentFile().getAbsolutePath());
				project = parser.parseProject(xmlFile);
			}
			catch(Exception e)
			{
				Log.e(TAG, "XML file could not be parsed", e);
				errorDialog("XML file could not be parsed: " + e.getLocalizedMessage()).show();
				return;
			}
		}
		else if(path.toLowerCase().endsWith(ExCiteSFileLoader.EXCITES_FILE_EXTENSION));
		{	
			//Extract & parse ExCiteS file...
			try
			{
				//Use /mnt/sdcard/ExCiteS/ as the basePath:
				ExCiteSFileLoader loader = new ExCiteSFileLoader(Environment.getExternalStorageDirectory().getAbsolutePath() + File.separatorChar + EXCITES_FOLDER);
				project = loader.load(new File(path));
			}
			catch(Exception e)
			{
				Log.e(TAG, "Could not load excites file", e);
				errorDialog("Could not load excites file: " + e.getLocalizedMessage()).show();
				return;
			}
		}
		
		//Check if we have a project object:
		if(project == null)
		{
			errorDialog("Invalid xml or excites file: " + path).show();
			return;
		}
		
		//Store the project object:
		try
		{
			dao.store(project);
		}
		catch(DuplicateException de)
		{
			errorDialog("Could not store project: " + de.getLocalizedMessage()).show();
			return;
		}
		
		//Update project list:
		populateProjectList();
	}

	// retrieve all parsed projects from db and populate list
	public void populateProjectList()
	{
		parsedProjects = dao.retrieveProjects();
		String[] values = new String[parsedProjects.size()];
		for(int i = 0; i < parsedProjects.size(); i++)
		{
			values[i] = parsedProjects.get(i).getName();
		}
		ArrayAdapter<String> adapter = new ArrayAdapter<String>(this, android.R.layout.simple_list_item_single_choice, android.R.id.text1, values);
		projectList.setAdapter(adapter);
	}

	// file filter
	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data)
	{
		super.onActivityResult(requestCode, resultCode, data);
		if(resultCode == Activity.RESULT_OK)
		{
			switch(requestCode)
			{
			case SETTINGS_REQUEST_IMPORT:
				// Get the result file path and import the settings
				String fileSource = data.getStringExtra(FileDialog.RESULT_PATH).trim();
				enterURL.setText(fileSource);
				enterURL.setSelection(fileSource.length());
				break;
			}
		}
	}

	// dialog to check whether it is desired to remove project
	public void removeDialog(View view)
	{
		if(projectList.getCheckedItemPosition() == -1)
		{
			AlertDialog NoSelection = errorDialog("Please select a project");
			NoSelection.show();
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
					})
					.setNegativeButton("Cancel", new DialogInterface.OnClickListener()
					{
						public void onClick(DialogInterface dialog, int whichButton)
						{
						}
					}).create();
			removeDialogBox.show();
		}
	}

	/**
	 * dialog shown when erroneous user interaction is detected
	 * @param message
	 * @return the dialog
	 */
	private AlertDialog errorDialog(String message)
	{
		AlertDialog Error = new AlertDialog.Builder(this).setTitle("Error").setMessage(message).setNeutralButton("OK", new DialogInterface.OnClickListener()
		{
			public void onClick(DialogInterface dialog, int whichButton)
			{
			}
		}).create();
		return Error;
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
	}

}
