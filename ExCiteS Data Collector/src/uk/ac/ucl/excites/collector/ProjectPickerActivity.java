package uk.ac.ucl.excites.collector;

import java.io.File;

import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.xml.ProjectParser;
import uk.ac.ucl.excites.collector.ui.filedialog.FileDialog;
import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.os.Environment;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.Window;
import android.view.WindowManager;
import android.widget.Button;
import android.widget.EditText;

public class ProjectPickerActivity extends Activity
{
	// Define some variables
	public static final int SETTINGS_REQUEST_IMPORT = 1;
	private File browseXMLFile;
	private EditText enterURL;
	private Button browseButton;

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		// Remove title
		requestWindowFeature(Window.FEATURE_NO_TITLE);
		// Hide soft keyboard on create
		getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_HIDDEN);
		setContentView(R.layout.activity_projectpicker);

		// Get View Elements
		enterURL = (EditText) findViewById(R.id.EnterURL);
		browseButton = (Button) findViewById(R.id.BrowseButton);
		
		// Browse Button
		browseButton.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
				Intent mIntent = new Intent(getBaseContext(), FileDialog.class);
				// Start from "/sdcard"
				mIntent.putExtra(FileDialog.START_PATH, Environment.getExternalStorageDirectory().getPath());

				// can user select directories or not
				mIntent.putExtra(FileDialog.CAN_SELECT_DIR, true);

				// set file filter
				mIntent.putExtra(FileDialog.FORMAT_FILTER, new String[] { "xml" });
				startActivityForResult(mIntent, SETTINGS_REQUEST_IMPORT);
			}
		});

		/*
		 * case R.id.settings_import:
		 * 
		 * break;
		 */

		
//		// run Parser
//		String xmlFilePath = Environment.getExternalStorageDirectory() + "/ExCiteSImagePicker/" + "ExCiteSCollectorXML.xml"; // path needs to be stored/passed as variable
//		File xmlFile = new File(xmlFilePath);
//		if(!xmlFile.exists())
//			throw new IllegalArgumentException("XML file not found (" + xmlFilePath + ").");
//		ProjectParser parser = new ProjectParser();
//		Project project = parser.parseProject(xmlFile);

		
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data)
	{
		super.onActivityResult(requestCode, resultCode, data);

		if(resultCode == FileDialog.RESULT_OK)
		{
			switch(requestCode)
			{
			case SETTINGS_REQUEST_IMPORT:
				// Get the result file path and import the settings
				String fileSource = data.getStringExtra(FileDialog.RESULT_PATH).trim();
				browseXMLFile = new File(fileSource);
				enterURL.setText(fileSource);
				enterURL.setSelection(fileSource.length());
				break;
			}
		}
	}

	@Override
	protected void onPause()
	{
		super.onPause();
	}

	@Override
	protected void onResume()
	{
		super.onResume();
	}
}
