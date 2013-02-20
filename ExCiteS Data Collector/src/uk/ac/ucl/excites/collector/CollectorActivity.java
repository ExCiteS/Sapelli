package uk.ac.ucl.excites.collector;

import java.util.Timer;
import java.util.TimerTask;

import uk.ac.ucl.excites.collector.project.db.DataAccess;
import uk.ac.ucl.excites.collector.project.model.Audio;
import uk.ac.ucl.excites.collector.project.model.Choice;
import uk.ac.ucl.excites.collector.project.model.Field;
import uk.ac.ucl.excites.collector.project.model.LocationField;
import uk.ac.ucl.excites.collector.project.model.Photo;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.ui.FieldView;
import android.app.Activity;
import android.os.Bundle;
import android.view.KeyEvent;
import android.widget.ImageButton;
import android.widget.LinearLayout;

/**
 * Main Collector activity
 *
 * @author mstevens, julia
 */
public class CollectorActivity extends Activity implements FieldView
{

	// UI
	ImageButton backButton;
	ImageButton cancelButton;
	
	// Dynamic fields:
	private DataAccess dao;
	private Project project;
	private ProjectController controller;
	private volatile Timer locationTimer;

	@Override
	protected void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);

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

		// UI set-up:
		setContentView(R.layout.activity_collector_fullscreen);

		// // Example: add choiceView to a RelativeLayout
		// RelativeLayout rl = new RelativeLayout(this);
		// RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(
		// RelativeLayout.LayoutParams.MATCH_PARENT,
		// RelativeLayout.LayoutParams.MATCH_PARENT);
		// ChoiceView choiceView = new ChoiceView(this);
		// rl.addView(choiceView, params);
		// setContentView(rl);
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
		LinearLayout ll = (LinearLayout) findViewById(R.id.activity_collector_fullscreen);
		
		if (showBack == true){
			backButton = new ImageButton(this);
			ll.addView(backButton);
		}
		
		if (showCancel == true){
			cancelButton = new ImageButton(this);
			ll.addView(cancelButton);
		}
		
		// Show/hide buttons:
		// TODO

		// Display the actual field (through double dispatch):
		field.setIn(this);
	}

	@Override
	public void setChoice(Choice cf)
	{
		// create new ChoiceView
		// add to layout
		// setChoice: ((ChoiceView) currentView).setChoice(cf, controller);
		//
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
	public void setLocation(LocationField fl)
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
		}, fl.getTimeoutS() * 1000);
	}

	public void stopLocationTimer()
	{
		if(locationTimer != null)
			locationTimer.cancel();
	}

}
