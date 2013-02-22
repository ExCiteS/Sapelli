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
import android.app.Activity;
import android.graphics.Color;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup.LayoutParams;
import android.view.Window;
import android.view.WindowManager;
import android.widget.Button;
import android.widget.LinearLayout;

/**
 * Main Collector activity
 * 
 * @author mstevens, julia
 */
public class CollectorActivity extends Activity implements FieldView
{

	// UI
	LinearLayout ll;
	Button backButton;
	Button cancelButton;

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
		String projectName = extras.getString("Project");
		String dbPath = extras.getString("Path");

		// Get DataAccess object
		dao = DataAccess.getInstance(dbPath);

		// Get Project object:
		project = dao.retrieveProject(projectName);// TODO error handling if not found

		// Set-up controller:
		controller = new ProjectController(project, dao, this);

		// start project
		controller.startProject();

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

	public void setField(Field field, boolean showCancel, boolean showBack, boolean showForward)
	{
		// set up UI
		ll = new LinearLayout(this);
		ll.setOrientation(LinearLayout.VERTICAL);
		ll.setLayoutParams(new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
		ll.setBackgroundColor(Color.BLACK);

		// set up Buttons TODO change Button to ImageButton
		LinearLayout buttonLayout = new LinearLayout(this);
		buttonLayout.setLayoutParams(new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT));
		buttonLayout.setWeightSum(1);
		LinearLayout.LayoutParams buttonParams = new LinearLayout.LayoutParams(155, 45, 0.5f);
		if(showBack == true)

		{
			backButton = new Button(this);
			backButton.setText("back");
			backButton.setLayoutParams(buttonParams);
			backButton.setOnClickListener(new OnClickListener()
			{
				public void onClick(View v)
				{
					controller.goBack();
				}
			});

			buttonLayout.addView(backButton);
		}

		if(showCancel == true)
		{
			cancelButton = new Button(this);
			cancelButton.setText("cancel");
			cancelButton.setLayoutParams(buttonParams);
			cancelButton.setOnClickListener(new OnClickListener()
			{
				public void onClick(View v)
				{
					controller.restartForm();
				}
			});

			buttonLayout.addView(cancelButton);

		}
		ll.addView(buttonLayout);

		// Display the actual field (through double dispatch):
		field.setIn(this);
	}

	@Override
	public void setChoice(Choice cf)
	{
		ChoiceView choiceView = new ChoiceView(this);
		choiceView.setChoice(cf, controller);

		LinearLayout choiceLayout = new LinearLayout(this);
		choiceLayout.setLayoutParams(new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT));

		choiceLayout.addView(choiceView);
		ll.addView(choiceLayout);
		setContentView(ll);

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
