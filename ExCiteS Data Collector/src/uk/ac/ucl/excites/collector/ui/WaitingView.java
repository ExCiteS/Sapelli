package uk.ac.ucl.excites.collector.ui;

import uk.ac.ucl.excites.collector.ProjectController;
import uk.ac.ucl.excites.collector.project.model.Field;
import android.content.Context;
import android.view.Gravity;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.ProgressBar;

/**
 * @author Julia, mstevens
 *
 */
public class WaitingView extends LinearLayout implements FieldView
{

	public WaitingView(Context context)
	{
		super(context);
		
		setGravity(Gravity.CENTER);
		addView(new ProgressBar(context, null, android.R.attr.progressBarStyleLarge));
	}

	@Override
	public void cancel()
	{
		//does nothing
	}

	@Override
	public void initialise(ProjectController controller, Field field)
	{
		//does nothing		
	}

	@Override
	public View getView()
	{
		return this;
	}
	
}
