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
import uk.ac.ucl.excites.collector.ui.ChoiceView;
import uk.ac.ucl.excites.collector.ui.ImageAdapter;
import android.app.Activity;
import android.graphics.Color;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.View;
import android.view.ViewGroup.LayoutParams;
import android.view.ViewTreeObserver.OnPreDrawListener;
import android.view.Window;
import android.view.WindowManager;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.GridView;
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
	LinearLayout rootLayout;
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

	public void setField(Field field, final boolean showCancel, final boolean showBack, boolean showForward)

	{
		// set up UI
		rootLayout = new LinearLayout(this);
		rootLayout.setOrientation(LinearLayout.VERTICAL);
		rootLayout.setLayoutParams(new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
		rootLayout.setBackgroundColor(Color.BLACK);

		// set up Buttons
		GridView buttonLayout = new GridView(this);

		if(showBack || showCancel)
		{
			ImageAdapter adapter = new ImageAdapter(this, 45);
			adapter.buttonsToDisplay(showBack, showCancel);
			if(showBack && showCancel)
				buttonLayout.setNumColumns(2);
			else
				buttonLayout.setNumColumns(1);
			buttonLayout.setHorizontalSpacing(10);
			buttonLayout.setVerticalSpacing(10);
			buttonLayout.setPadding(0, 0, 0, 10);
			buttonLayout.setAdapter(adapter);
			buttonLayout.setOnItemClickListener(new OnItemClickListener()
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

			rootLayout.addView(buttonLayout);
		}

		// Display the actual field (through double dispatch):
		field.setIn(this);

		setContentView(rootLayout);
	}

	@Override
	public void setChoice(final Choice cf)
	{
		final ChoiceView choiceView = new ChoiceView(this);
		rootLayout.addView(choiceView, new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
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
