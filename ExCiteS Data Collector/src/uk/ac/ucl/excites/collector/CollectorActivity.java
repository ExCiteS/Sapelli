package uk.ac.ucl.excites.collector;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

import uk.ac.ucl.excites.collector.project.db.DataAccess;
import uk.ac.ucl.excites.collector.project.model.Audio;
import uk.ac.ucl.excites.collector.project.model.Choice;
import uk.ac.ucl.excites.collector.project.model.Field;
import uk.ac.ucl.excites.collector.project.model.LocationField;
import uk.ac.ucl.excites.collector.project.model.OrientationField;
import uk.ac.ucl.excites.collector.project.model.Photo;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.ui.FieldView;
import uk.ac.ucl.excites.collector.ui.AudioView;
import uk.ac.ucl.excites.collector.ui.BaseActivity;
import uk.ac.ucl.excites.collector.ui.ButtonView;
import uk.ac.ucl.excites.collector.project.util.FileHelpers;
import uk.ac.ucl.excites.collector.ui.ChoiceView;
import uk.ac.ucl.excites.collector.util.Debug;
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
import android.view.Gravity;
import android.view.KeyEvent;
import android.view.View;
import android.view.ViewGroup.LayoutParams;
import android.view.ViewTreeObserver.OnPreDrawListener;
import android.view.Window;
import android.view.WindowManager;
import android.widget.LinearLayout;
import android.widget.ProgressBar;

/**
 * Main Collector activity
 * 
 * @author mstevens, julia, Michalis Vitos
 */
public class CollectorActivity extends BaseActivity implements FieldView
{

	//STATICS--------------------------------------------------------
	static private final String TAG = "CollectorActivity";

	static public final String PARAMETER_PROJECT_NAME = "Project_name";
	static public final String PARAMETER_PROJECT_VERSION = "Project_version";
	static public final String PARAMETER_DB_FOLDER_PATH = "DBFolderPath";

	static private final String TEMP_PHOTO_PREFIX = "tmpPhoto";
    static private final String TEMP_PHOTO_SUFFIX = ".tmp";
    static private final String TEMP_PHOTO_LOCATION_KEY = "tmpPhotoLocation";
    
	// Request codes for returning data from intents
	static public final int RETURN_PHOTO_CAPTURE = 1;
	static public final int RETURN_VIDEO_CAPTURE = 2;
	static public final int RETURN_AUDIO_CAPTURE = 3;
    
	//DYNAMICS-------------------------------------------------------
	
	// UI
	private LinearLayout rootLayout;
	private ButtonView buttonView;
	private View fieldView;
	private int viewWidth;

	// Dynamic fields:
	private DataAccess dao;
	private Project project;
	private ProjectController controller;
	private volatile Timer locationTimer;

	// Temp location to save a photo
	private static String tmpPhotoLocation;

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);

		//Retrieve the tmpPhotoLocation for the saved state
		if(savedInstanceState != null)
			tmpPhotoLocation = savedInstanceState.getString(TEMP_PHOTO_LOCATION_KEY);

		//Check if there is an SD Card, otherwise inform the user and finish the activity
		if(!SDCard.isExternalStorageWritable())
		{
			errorDialog("ExCiteS needs an SD card in order to function. Please insert one and restart the application.", true).show(); //will exit the activity after used clicks OK
			return;
		}
			
		// get project name and path from bundle
		Bundle extras = getIntent().getExtras();
		String projectName = extras.getString(PARAMETER_PROJECT_NAME);
		int projectVersion = extras.getInt(PARAMETER_PROJECT_VERSION);
		String dbFolderPath = extras.getString(PARAMETER_DB_FOLDER_PATH);

		// Get DataAccess object
		dao = DataAccess.getInstance(dbFolderPath);

		// Get Project object:
		project = dao.retrieveProject(projectName, projectVersion);
		if(project == null)
		{
			errorDialog("Could not find project: " + projectName + "(version " + projectVersion + ").", true).show(); //will quit activity after "OK" is clicked
			return;
		}

		//UI setup:
		requestWindowFeature(Window.FEATURE_NO_TITLE);																	// Remove title
		setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);												// Lock the orientation
		getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN, WindowManager.LayoutParams.FLAG_FULLSCREEN);	// Set to FullScreen
		
		// Set up root layout UI
		rootLayout = new LinearLayout(this);
		rootLayout.setOrientation(LinearLayout.VERTICAL);
		rootLayout.setLayoutParams(new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
		rootLayout.setBackgroundColor(Color.BLACK);
		setContentView(rootLayout);
		
		// Set-up controller:
		controller = new ProjectController(project, dao, this);
	}
	
	@Override
	protected void onStart()
	{
		super.onStart();
		
		if(controller != null)
			controller.startProject();
		else
			errorDialog("Could not start project, controller is not set-up.", true); //will exit the activity after "OK" is clicked
	}

	/**
	 * Handle device key presses (mostly disabling them)
	 */
	@Override
	public boolean onKeyDown(int keyCode, KeyEvent event)
	{
		switch (keyCode)
		{
		case KeyEvent.KEYCODE_BACK:
			controller.goBack(); //TODO maybe make this optional?
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
		switch (keyCode)
		{
		case KeyEvent.KEYCODE_VOLUME_DOWN:
			return true;
		case KeyEvent.KEYCODE_VOLUME_UP:
			return true;
		}
		return super.onKeyUp(keyCode, event);
	}	

	/**
	 * Called from the controller to set-up the UI for a given Field of the current Form
	 * Uses double-dispatch to specialise based on Field type.
	 * 
	 * @param field
	 * @param showCancel
	 * @param showBack
	 * @param showForward
	 */
	public void setField(Field field, final boolean showCancel, final boolean showBack, final boolean showForward)
	{
		// set up Buttons
		if(showBack || showCancel || showForward)
		{
			if(buttonView == null)
			{
				buttonView = new ButtonView(this);
				rootLayout.addView(buttonView);
			}
	
			buttonView.setButtonView(controller, viewWidth, showCancel, showBack, showForward);

		}
		else if(buttonView != null)
		{
			rootLayout.removeView(buttonView);
			buttonView = null;
		}
		// Display the actual field (through double dispatch):
		field.setIn(this);
	}

	/**
	 * Set the field view and removes any previous one from the screen
	 * 
	 * @param fieldView
	 */
	private void setFieldView(View fieldView)
	{
		if(this.fieldView != null)
			rootLayout.removeView(this.fieldView); // throw away the old fieldField
		this.fieldView = fieldView;
		rootLayout.addView(fieldView, new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
	}

	@Override
	public void setChoice(final Choice cf)
	{
		final ChoiceView choiceView = new ChoiceView(this);
		setFieldView(choiceView);
		choiceView.getViewTreeObserver().addOnPreDrawListener(new OnPreDrawListener()
		{
			public boolean onPreDraw()
			{
				choiceView.setChoice(cf, controller);
				viewWidth = choiceView.getWidth();
				choiceView.getViewTreeObserver().removeOnPreDrawListener(this); // avoid endless loop
				return false;
			}
		});
	}
	
	@Override
	public void setAudio(final Audio af)
	{
		final AudioView audioView = new AudioView(this);
		setFieldView(audioView);
		audioView.getViewTreeObserver().addOnPreDrawListener(new OnPreDrawListener()
		{
			public boolean onPreDraw()
			{
				audioView.setAudioView(af, controller);
				audioView.getViewTreeObserver().removeOnPreDrawListener(this); // avoid endless loop
				return false;
			}
		});
	}

	/** 
	 * Calls on the built-in camera application of the phone to let the user take a photo 
	 * 
	 * Note:
	 * There is a known issue regarding the returned intent from MediaStore.ACTION_IMAGE_CAPTURE:
	 *   - https://code.google.com/p/android/issues/detail?id=1480
	 *   - http://stackoverflow.com/questions/6530743/beautiful-way-to-come-over-bug-with-action-image-capture
	 *   - http://stackoverflow.com/questions/1910608/android-action-image-capture-intent/1932268#1932268
	 *   - http://stackoverflow.com/questions/12952859/capturing-images-with-mediastore-action-image-capture-intent-in-android
	 *   
	 * As a workaround we create a temporary file for the image to be saved and afterwards (in cameraDone()) we rename the file to the correct name.
	 */
	@Override
	public void setPhoto(Photo pf)
	{
		// Check if the device is able to handle Photo Intents
		if(!isIntentAvailable(this, MediaStore.ACTION_IMAGE_CAPTURE))
		{	//Device cannot take photos
			Log.i(TAG, "Cannot take photo due to device limitation.");
			controller.photoDone(false); //skip the Photo field
			return;
		}
		//else...
		try
		{	
			// Check if data path is accessible
			if(!FileHelpers.createFolder(project.getDataPath()))
				throw new IOException("Data path (" + project.getDataPath() + ") is not accessible.");

			// Set up temp file
			File parentDir = new File(project.getDataPath()); // The file is saved to the projects data folder
			File tmpPhotoFile = File.createTempFile(TEMP_PHOTO_PREFIX, TEMP_PHOTO_SUFFIX, parentDir);
			tmpPhotoLocation = tmpPhotoFile.getAbsolutePath();
			
			// Save the photo to the tmp location
			Intent takePictureIntent = new Intent(MediaStore.ACTION_IMAGE_CAPTURE); //Get the intent
			takePictureIntent.putExtra(MediaStore.EXTRA_OUTPUT, Uri.fromFile(tmpPhotoFile));
			startActivityForResult(takePictureIntent, RETURN_PHOTO_CAPTURE);
		}
		catch(Exception e)
		{
			Debug.e("setPhoto() error: " + e.toString(), e);
			controller.photoDone(false);
		}
	}
	
	private void cameraDone(int resultCode)
	{
		if(resultCode == RESULT_CANCELED)
		{
			FileHelpers.deleteFile(tmpPhotoLocation); // Delete the tmp file from the device
			controller.photoDone(false);
		}
		else if(resultCode == RESULT_OK)
		{
			// TODO
			// get device id
			// decide on suffix or not suffix
			String photoFilename = "DeviceId-" + System.currentTimeMillis();

			// Create the files
			File from = new File(tmpPhotoLocation);
			File to = new File(project.getDataPath() + File.separator + photoFilename);

			// Rename the file
			from.renameTo(to);

			// Call Controller
			controller.photoDone(true);
		}
		else
		{	//this should not happen
			controller.photoDone(false);
		}
	}
	
	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data)
	{
		super.onActivityResult(requestCode, resultCode, data);
		switch(requestCode)
		{
			case RETURN_PHOTO_CAPTURE : cameraDone(resultCode); break;
			case RETURN_VIDEO_CAPTURE : /* TODO */; break;
			//more later?
			default : return;
		}	
	}

	@Override
	public void onSaveInstanceState(Bundle bundle)
	{
		super.onSaveInstanceState(bundle);

		// If the app is taking a photo, save the tmpPhotoLocation
		if(tmpPhotoLocation != null)
			bundle.putString(TEMP_PHOTO_LOCATION_KEY, tmpPhotoLocation);
	}

	public static boolean isIntentAvailable(Context context, String action)
	{
		final PackageManager packageManager = context.getPackageManager();
		final Intent intent = new Intent(action);
		List<ResolveInfo> list = packageManager.queryIntentActivities(intent, PackageManager.MATCH_DEFAULT_ONLY);
		return list.size() > 0;
	}

	@Override
	public void setLocation(LocationField lf)
	{
		// Show waiting view
		LinearLayout waitingView = new LinearLayout(this);
		waitingView.setGravity(Gravity.CENTER);
		waitingView.addView(new ProgressBar(this, null, android.R.attr.progressBarStyleLarge));
		rootLayout.addView(waitingView, new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));

		// Start timeout counter
		locationTimer = new Timer();
		locationTimer.schedule(new TimerTask()
		{
			@Override
			public void run()
			{ // time's up!
				controller.goForward();

			}
		}, lf.getTimeoutS() * 1000);
	}

	public void stopLocationTimer()
	{
		if(locationTimer != null)
			locationTimer.cancel();
	}

	@Override
	public void setOrientation(OrientationField of)
	{
		// TODO Auto-generated method stub

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
