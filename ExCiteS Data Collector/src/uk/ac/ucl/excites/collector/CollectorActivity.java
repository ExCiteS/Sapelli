package uk.ac.ucl.excites.collector;

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
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.graphics.Color;
import android.os.Bundle;
import android.util.Log;
import android.view.Gravity;
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
import android.widget.ProgressBar;

/**
 * Main Collector activity
 * 
 * @author mstevens, julia
 */
public class CollectorActivity extends Activity implements FieldView
{

	static private final String TAG = "CollectorActivity";

	static public final String PARAMETER_PROJECT_NAME = "Project_name";
	static public final String PARAMETER_PROJECT_VERSION = "Project_version";
	static public final String PARAMETER_DB_FOLDER_PATH = "DBFolderPath";
	
	// UI
	private LinearLayout rootLayout;
	private GridView buttonsGrid;
	private View fieldView;

	// Dynamic fields:
	private DataAccess dao;
	private Project project;
	private ProjectController controller;
	private volatile Timer locationTimer;

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);

		// Remove title
		requestWindowFeature(Window.FEATURE_NO_TITLE);

		// Set to FullScreen
		getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN, WindowManager.LayoutParams.FLAG_FULLSCREEN);

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
			(new AlertDialog.Builder(this).setTitle("Error").setMessage("Could not find project: " + projectName + "(version " + projectVersion + ").").setNeutralButton("OK", new DialogInterface.OnClickListener()
			{
				public void onClick(DialogInterface dialog, int whichButton) {}
			}).create()).show();
			return;
		}
		
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
			ImageAdapter adapter = new ImageAdapter(this, project, 45);
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
		ChoiceView choiceView = new ChoiceView(this);
		choiceView.setChoice(cf, controller);
		setFieldView(choiceView);
//		choiceView.getViewTreeObserver().addOnPreDrawListener(new OnPreDrawListener()
//		{
//			public boolean onPreDraw()
//			{
//				choiceView.setChoice(cf, controller);
//				choiceView.getViewTreeObserver().removeOnPreDrawListener(this); // avoid endless loop
//				return true;
//			}
//		});
	}

	@Override
	public void setPhoto(Photo pf)
	{
		// TODO Auto-generated method stub

	}

	@Override
	public void setAudio(Audio af)
	{
		// TODO Auto-generated method stub

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
