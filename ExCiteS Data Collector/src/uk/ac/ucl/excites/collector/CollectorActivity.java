package uk.ac.ucl.excites.collector;

import java.io.File;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

import uk.ac.ucl.excites.collector.project.db.DataAccess;
import uk.ac.ucl.excites.collector.project.model.AudioField;
import uk.ac.ucl.excites.collector.project.model.CancelField;
import uk.ac.ucl.excites.collector.project.model.ChoiceField;
import uk.ac.ucl.excites.collector.project.model.EndField;
import uk.ac.ucl.excites.collector.project.model.Field;
import uk.ac.ucl.excites.collector.project.model.LocationField;
import uk.ac.ucl.excites.collector.project.model.OrientationField;
import uk.ac.ucl.excites.collector.project.model.PhotoField;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.ui.CollectorUI;
import uk.ac.ucl.excites.collector.ui.AudioView;
import uk.ac.ucl.excites.collector.ui.BaseActivity;
import uk.ac.ucl.excites.collector.ui.ButtonView;
import uk.ac.ucl.excites.collector.ui.CameraView;
import uk.ac.ucl.excites.collector.ui.ChoiceView;
import uk.ac.ucl.excites.collector.ui.FieldView;
import uk.ac.ucl.excites.collector.ui.WaitingView;
import uk.ac.ucl.excites.collector.util.SDCard;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.graphics.Color;
import android.net.Uri;
import android.os.Bundle;
import android.provider.MediaStore;
import android.util.Log;
import android.view.KeyEvent;
import android.view.ViewGroup.LayoutParams;
import android.view.Window;
import android.view.WindowManager;
import android.widget.LinearLayout;

/**
 * Main Collector activity
 * 
 * @author mstevens, julia, Michalis Vitos
 */
public class CollectorActivity extends BaseActivity implements CollectorUI
{

	// STATICS--------------------------------------------------------
	static private final String TAG = "CollectorActivity";

	static public final String PARAMETER_PROJECT_NAME = "Project_name";
	static public final String PARAMETER_PROJECT_VERSION = "Project_version";
	static public final String PARAMETER_DB_FOLDER_PATH = "DBFolderPath";

	static private final String TEMP_PHOTO_PREFIX = "tmpPhoto";
	static private final String TEMP_PHOTO_SUFFIX = ".tmp";
	static private final String TEMP_PHOTO_PATH_KEY = "tmpPhotoPath";

	static private final int BUTTONS_VIEW_ID = 0;
	static private final int FIELD_VIEW_ID = 1;

	// Request codes for returning data from intents
	static public final int RETURN_PHOTO_CAPTURE = 1;
	static public final int RETURN_VIDEO_CAPTURE = 2;
	static public final int RETURN_AUDIO_CAPTURE = 3;
	
	private static final long TIMEOUT_MS = 5 * 60 * 1000; //timeout after 5 minutes

	// DYNAMICS-------------------------------------------------------

	// UI
	private LinearLayout rootLayout;
	private ButtonView buttonView;
	private FieldView fieldView;

	// Dynamic fields:
	private DataAccess dao;
	private Project project;
	private ProjectController controller;

	// Temp location to save a photo
	private File tmpPhotoFile;

	// Project Info
	private String projectName;
	private String projectVersion;
	private String dbFolderPath;
	
	// Timeout:
	protected boolean pausedForActivityResult = false;
	protected Timer pauseTimer = null;
	protected boolean timedOut = false;

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);

		// Retrieve the tmpPhotoLocation for the saved state
		if(savedInstanceState != null)
			tmpPhotoFile = new File(savedInstanceState.getString(TEMP_PHOTO_PATH_KEY));

		// Check if there is an SD Card, otherwise inform the user and finish the activity
		if(!SDCard.isExternalStorageWritable())
		{ // show error (activity will be exited after used clicks OK in the dialog):
			errorDialog("ExCiteS needs an SD card in order to function. Please insert one and restart the application.", true).show();
			return;
		}

		// get project name and path from bundle
		loadProjectInfo();

		// Get DataAccess object
		dao = DataAccess.getInstance(dbFolderPath);

		// Get Project object:
		project = dao.retrieveProject(projectName, projectVersion);
		if(project == null)
		{	// show error (activity will be exited after used clicks OK in the dialog):
			errorDialog("Could not find project: " + projectName + " (version " + projectVersion + ").", true).show();
			return;
		}

		// UI setup:
		requestWindowFeature(Window.FEATURE_NO_TITLE); // Remove title
		setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT); // Lock the orientation
		getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN, WindowManager.LayoutParams.FLAG_FULLSCREEN); // Set to FullScreen

		// Set-up root layout
		rootLayout = new LinearLayout(this);
		rootLayout.setOrientation(LinearLayout.VERTICAL);
		rootLayout.setLayoutParams(new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
		rootLayout.setBackgroundColor(Color.BLACK);
		setContentView(rootLayout);

		// Set-up buttonView
		buttonView = new ButtonView(this);
		buttonView.setId(BUTTONS_VIEW_ID);
		rootLayout.addView(buttonView, new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT));

		// Set-up controller:
		controller = new ProjectController(project, dao, this);

		// Start project:
		controller.startProject();
	}

	private void loadProjectInfo()
	{
		// Get extra info and check if there is a shortcut info there
		Bundle extras = getIntent().getExtras();
		if(extras != null && extras.containsKey(ProjectPickerActivity.SHORTCUT_PROJECT_NAME))
		{
			// Get the shortcut name and version
			projectName = extras.getString(ProjectPickerActivity.SHORTCUT_PROJECT_NAME);
			projectVersion = extras.getString(ProjectPickerActivity.SHORTCUT_PROJECT_VERSION);
			if(projectVersion == null)
				projectVersion = Project.DEFAULT_VERSION;
			dbFolderPath = extras.getString(ProjectPickerActivity.SHORTCUT_PROJECT_DB);
		}
		else if(extras.containsKey(PARAMETER_PROJECT_NAME))
		{
			projectName = extras.getString(PARAMETER_PROJECT_NAME);
			projectVersion = extras.getString(PARAMETER_PROJECT_VERSION);
			dbFolderPath = extras.getString(PARAMETER_DB_FOLDER_PATH);
		}
	}

	/**
	 * Handle device key presses (mostly disabling them)
	 */
	@Override
	public boolean onKeyDown(int keyCode, KeyEvent event)
	{
		switch(keyCode)
		{
		case KeyEvent.KEYCODE_BACK:
			controller.goBack(); // TODO maybe make this optional?
			return true;
		case KeyEvent.KEYCODE_DPAD_RIGHT:
			return true;
		case KeyEvent.KEYCODE_DPAD_LEFT:
			return true;
		case KeyEvent.KEYCODE_VOLUME_DOWN:
			return true;
		case KeyEvent.KEYCODE_VOLUME_UP:
			return true;
		}
		return super.onKeyDown(keyCode, event);
	}

	/**
	 * Handle device key presses (disabling them)
	 */
	@Override
	public boolean onKeyUp(int keyCode, KeyEvent event)
	{
		switch(keyCode)
		{
		case KeyEvent.KEYCODE_VOLUME_DOWN:
			return true;
		case KeyEvent.KEYCODE_VOLUME_UP:
			return true;
		}
		return super.onKeyUp(keyCode, event);
	}

	/**
	 * Called from the controller to set-up the UI for a given Field of the current Form Uses double-dispatch to specialise based on Field type.
	 * 
	 * @param field
	 */
	public void setField(Field field)
	{
		// briefly disable the buttons:
		buttonView.disable();
		// Remove previous field view
		removeFieldView();
		// Update buttons
		buttonView.update(controller);
		// Display the actual field (through double dispatch):
		field.setIn(this);
		// enable the buttons:
		buttonView.enable();
	}

	private void removeFieldView()
	{
		if(fieldView != null)
		{
			fieldView.cancel(); // to stop audio recording, close camera, ...
			rootLayout.removeView(fieldView.getView()); // throw away the old fieldField
			fieldView = null;
		}
	}

	/**
	 * Set the field view and removes any previous one from the screen
	 * 
	 * @param fieldView
	 */
	private void setFieldView(FieldView fieldView, Field field)
	{
		removeFieldView(); // just in case
		this.fieldView = fieldView;
		fieldView.getView().setId(FIELD_VIEW_ID);
		fieldView.initialise(controller, field); // !!!
		rootLayout.addView(fieldView.getView(), new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
	}

	@Override
	public void setEndField(EndField endF)
	{
		controller.endForm();
	}

	@Override
	public void setCancelField(CancelField cancelF)
	{
		controller.cancelAndRestartForm();
	}

	@Override
	public void setChoice(ChoiceField cf)
	{
		setFieldView(new ChoiceView(this), cf);
	}

	@Override
	public void setAudio(AudioField af)
	{
		setFieldView(new AudioView(this), af);
	}

	@Override
	public void setLocation(LocationField lf)
	{
		// Show waiting view
		setFieldView(new WaitingView(this), lf); // will start timer
	}

	@Override
	public void setOrientation(OrientationField of)
	{
		// do nothing (for now?)
	}

	@Override
	public void setPhoto(PhotoField pf)
	{
		if(!pf.isUseNativeApp())
		{ // Use built-in camera feature:
			setFieldView(new CameraView(this), pf);
		}
		else
		{ /*
		 * Use native/Android camera app
		 * 
		 * Note: There is a known issue regarding the returned intent from MediaStore.ACTION_IMAGE_CAPTURE: -
		 * https://code.google.com/p/android/issues/detail?id=1480 -
		 * http://stackoverflow.com/questions/6530743/beautiful-way-to-come-over-bug-with-action-image-capture -
		 * http://stackoverflow.com/questions/1910608/android-action-image-capture-intent/1932268#1932268 -
		 * http://stackoverflow.com/questions/12952859/capturing-images-with-mediastore-action-image-capture-intent-in-android
		 * 
		 * As a workaround we create a temporary file for the image to be saved and afterwards (in cameraDone()) we rename the file to the correct name.
		 */
			if(!isIntentAvailable(this, MediaStore.ACTION_IMAGE_CAPTURE)) // check if the device is able to handle PhotoField Intents
			{ // Device cannot take photos
				Log.i(TAG, "Cannot take photo due to device limitation.");
				controller.mediaDone(null); // skip the PhotoField field (pass null to indicate no file was created)
			}
			else
			{ // Device can take photos
				tmpPhotoFile = null;
				try
				{
					// Set up temp file (in the projects data folder)
					tmpPhotoFile = File.createTempFile(TEMP_PHOTO_PREFIX, TEMP_PHOTO_SUFFIX, project.getTempFolder()); //getTempFolder() does the necessary IO checks
					//Set-up intent:
					Intent takePictureIntent = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
					takePictureIntent.putExtra(MediaStore.EXTRA_OUTPUT, Uri.fromFile(tmpPhotoFile));
					//Fire intent:
					pausedForActivityResult = true;
					startActivityForResult(takePictureIntent, RETURN_PHOTO_CAPTURE);
				}
				catch(Exception e)
				{
					if(tmpPhotoFile != null && tmpPhotoFile.exists())
						tmpPhotoFile.delete();
					Log.e(TAG, "setPhoto() error", e);
					controller.mediaDone(null);
				}
			}
		}
	}

	private void cameraDone(int resultCode)
	{
		if(resultCode == RESULT_OK)
		{
			if(tmpPhotoFile != null && tmpPhotoFile.exists())
			{
				try
				{	// Rename the file & pass it to the controller
					File newPhoto = ((PhotoField) controller.getCurrentField()).getNewTempFile(controller.getCurrentRecord());
					tmpPhotoFile.renameTo(newPhoto);
					controller.mediaDone(newPhoto);
				}
				catch(Exception e)
				{ // could not rename the file
					tmpPhotoFile.delete();
					controller.mediaDone(null);
				}
			}
			else
				controller.mediaDone(null);
		}
		else
		// if(resultCode == RESULT_CANCELED)
		{
			if(tmpPhotoFile != null && tmpPhotoFile.exists())
				tmpPhotoFile.delete(); // Delete the tmp file from the device
			controller.mediaDone(null);
		}
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data)
	{
		super.onActivityResult(requestCode, resultCode, data);
		pausedForActivityResult = false;
		switch(requestCode)
		{
		case RETURN_AUDIO_CAPTURE:
			/* TODO */
			break;
		case RETURN_PHOTO_CAPTURE:
			cameraDone(resultCode);
			break;
		case RETURN_VIDEO_CAPTURE:
			/* TODO */
			break;
		default:
			return;
		}
	}

	@Override
	public void onSaveInstanceState(Bundle bundle)
	{
		super.onSaveInstanceState(bundle);

		// If the app is taking a photo, save the tmpPhotoLocation
		if(tmpPhotoFile != null)
			bundle.putString(TEMP_PHOTO_PATH_KEY, tmpPhotoFile.getAbsolutePath());
	}

	public static boolean isIntentAvailable(Context context, String action)
	{
		final PackageManager packageManager = context.getPackageManager();
		final Intent intent = new Intent(action);
		List<ResolveInfo> list = packageManager.queryIntentActivities(intent, PackageManager.MATCH_DEFAULT_ONLY);
		return list.size() > 0;
	}

	@Override
	protected void onPause()
	{ 
		//close database
		dao.closeDB();
		//set timeout timer:
		if(!pausedForActivityResult)
		{
			pauseTimer = new Timer();
			timedOut = false;
			pauseTimer.schedule(new TimerTask()
				{
					@Override
					public void run()
					{	//time's up!
						if(fieldView != null)
							fieldView.cancel();
						controller.cancelAndStop();
						timedOut = true;
						Log.i(TAG, "Time-out reached");
					}
				}, TIMEOUT_MS);
		}
		//super:
		super.onPause();
	}

	@Override
	protected void onResume()
	{
		if(pausedForActivityResult)
			pausedForActivityResult = false;
		else
		{
			//restart project if needed:
			if(timedOut)
			{	//restart project:
				controller.startProject();
				timedOut = false;
			}
			else
			{	//cancel timer if needed:
				if(pauseTimer != null)
					pauseTimer.cancel();
			}
		}
		// open database
		dao.openDB();
		//super:
		super.onResume();
	}

	@Override
	protected void onDestroy()
	{
		//clean up:
		if(fieldView != null)
			fieldView.cancel();
		if(pauseTimer != null)
			pauseTimer.cancel();
		if(controller != null)
			controller.cancelAndStop();
		//super:
		super.onDestroy();
	}

}
