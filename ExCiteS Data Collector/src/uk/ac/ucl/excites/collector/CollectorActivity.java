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
import uk.ac.ucl.excites.collector.ui.ChoiceView;
import uk.ac.ucl.excites.collector.ui.ImageAdapter;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.graphics.Color;
import android.net.Uri;
import android.os.Bundle;
import android.os.Environment;
import android.provider.MediaStore;
import android.util.Log;
import android.view.KeyEvent;
import android.view.View;
import android.view.ViewGroup.LayoutParams;
import android.view.ViewTreeObserver.OnPreDrawListener;
import android.view.Window;
import android.view.WindowManager;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.GridView;
import android.widget.LinearLayout;

/**
 * Main Collector activity
 * 
 * @author mstevens, julia
 */
public class CollectorActivity extends Activity implements FieldView
{

	static private final String TAG = "CollectorActivity";

	// UI
	private LinearLayout rootLayout;
	private GridView buttonsGrid;
	private View fieldView;

	// Dynamic fields:
	private DataAccess dao;
	private Project project;
	private ProjectController controller;
	private volatile Timer locationTimer;

	// Request codes for returning data from intents
	public static final int PHOTO_CAPTURE = 1;
	public static final int VIDEO_CAPTURE = 2;
	public static final int AUDIO_CAPTURE = 3;

	// Temp location to save a photo
	private static String tmpPhotoLocation;

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);

		// Retrieve the tmpPhotoLocation for the saved state
		if(savedInstanceState != null)
			tmpPhotoLocation = savedInstanceState.getString("tmpPhotoLocation");

		// Remove title
		requestWindowFeature(Window.FEATURE_NO_TITLE);

		// Set to FullScreen
		getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN, WindowManager.LayoutParams.FLAG_FULLSCREEN);

		// get project name and path from bundle
		Bundle extras = getIntent().getExtras();
		String projectName = extras.getString("Project");
		String dbPath = extras.getString("Path");

		// Get DataAccess object
		dao = DataAccess.getInstance(dbPath);

		// Get Project object:
		project = dao.retrieveProject(projectName);// TODO error handling if not found

		// Set-up controller:
		controller = new ProjectController(project, dao, this);

		// Set up root layout UI
		rootLayout = new LinearLayout(this);
		rootLayout.setOrientation(LinearLayout.VERTICAL);
		rootLayout.setLayoutParams(new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
		rootLayout.setBackgroundColor(Color.BLACK);
		setContentView(rootLayout);

		// Start project
		controller.startProject(); // keep this as the last statement of the method!
	}

	/**
	 * Maybe make this optional?
	 */
	@Override
	public boolean onKeyDown(int keyCode, KeyEvent event)
	{
		switch(keyCode)
		{
		case KeyEvent.KEYCODE_BACK:
			controller.goBack();
			return true;
		}
		return super.onKeyDown(keyCode, event);
	}

	public void setField(Field field, final boolean showCancel, final boolean showBack, boolean showForward)
	{
		// set up Buttons
		if(showBack || showCancel)
		{
			if(buttonsGrid == null)
			{
				buttonsGrid = new GridView(this);
				rootLayout.addView(buttonsGrid);
			}

			ImageAdapter adapter = new ImageAdapter(this, 45);
			adapter.buttonsToDisplay(showBack, showCancel);
			if(showBack && showCancel)
				buttonsGrid.setNumColumns(2);
			else
				buttonsGrid.setNumColumns(1);
			buttonsGrid.setHorizontalSpacing(10);
			buttonsGrid.setVerticalSpacing(10);
			buttonsGrid.setPadding(0, 0, 0, 10);
			buttonsGrid.setAdapter(adapter);
			buttonsGrid.setOnItemClickListener(new OnItemClickListener()
			{
				@Override
				public void onItemClick(AdapterView<?> parent, View v, int position, long id)
				{
					if(showBack && showCancel)
					{
						if(position == 0)
							controller.goBack();
						if(position == 1)
							controller.restartForm();
						return;
					}
					if(showBack)
						controller.goBack();
					if(showCancel)
						controller.restartForm();
				}
			});
		}
		else if(buttonsGrid != null)
		{
			rootLayout.removeView(buttonsGrid);
			buttonsGrid = null;
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
				choiceView.getViewTreeObserver().removeOnPreDrawListener(this); // avoid endless loop
				return true;
			}
		});
	}

	@Override
	public void setPhoto(Photo pf)
	{
		// Define the temp name
		final String PHOTO_PREFIX = "tmpPhoto";
		final String PHOTO_SUFFIX = ".tmp";
		File tmpPhotoFile = null;

		// Create an image file
		try
		{
			tmpPhotoFile = File.createTempFile(PHOTO_PREFIX, PHOTO_SUFFIX);
			tmpPhotoLocation = tmpPhotoFile.getAbsolutePath();
		}
		catch(IOException e)
		{
			Log.e("ExCiteS_Debug", "setPhoto() error: " + e.toString());
		}

		// Check if the device is able to handle Photo Intents
		if(isIntentAvailable(this, MediaStore.ACTION_IMAGE_CAPTURE))
		{
			Intent takePictureIntent = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
			// Save the photo to the tmp location
			takePictureIntent.putExtra(MediaStore.EXTRA_OUTPUT, Uri.fromFile(tmpPhotoFile));
			startActivityForResult(takePictureIntent, PHOTO_CAPTURE);
		}
	}

	@Override
	public void setAudio(Audio af)
	{
		// TODO Auto-generated method stub

	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data)
	{
		// TODO Auto-generated method stub
		super.onActivityResult(requestCode, resultCode, data);

		if(resultCode == RESULT_CANCELED)
		{
			switch(requestCode)
			{
			case PHOTO_CAPTURE:
				// TODO Delete the tmpFile
				Log.i("ExCiteS_Debug", "PHOTO_CAPTURE - Canceled and file is in" + tmpPhotoLocation);
				break;
			}
		}

		if(resultCode == Activity.RESULT_OK)
		{
			switch(requestCode)
			{
			case PHOTO_CAPTURE:

				Log.i("ExCiteS_Debug", "PHOTO_CAPTURE - The photo is in: " + tmpPhotoLocation);

				// TODO Move the file from temp location to Projects Folder

				// TODO
				// get device id
				// decide on suffix or not suffix
				// get images folder
				// final String JPEG_FILE_PREFIX = "deviceID-" + String.valueOf(System.currentTimeMillis());

				// TODO Call Controller
				controller.photoDone(true);

				break;

			}
		}
	}

	@Override
	public void onSaveInstanceState(Bundle bundle)
	{
		super.onSaveInstanceState(bundle);

		// If the app is taking a photo, save the tmpPhotoLocation
		if(tmpPhotoLocation != null)
			bundle.putString("tmpPhotoLocation", tmpPhotoLocation);
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

}
