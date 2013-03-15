package uk.ac.ucl.excites.collector.ui;

import java.util.Timer;
import java.util.TimerTask;

import uk.ac.ucl.excites.collector.ProjectController;
import uk.ac.ucl.excites.collector.project.model.Field;
import uk.ac.ucl.excites.collector.project.util.Timeoutable;
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

	protected Timer timeoutCounter = null;
	
	public WaitingView(Context context)
	{
		super(context);
		
		setGravity(Gravity.CENTER);
		addView(new ProgressBar(context, null, android.R.attr.progressBarStyleLarge));
	}

	@Override
	public void cancel()
	{
		if(timeoutCounter != null)
			timeoutCounter.cancel();
		//else: do nothing
	}

	@Override
	public void initialise(final ProjectController controller, final Field field)
	{
		if(field instanceof Timeoutable)
		{
			//Start timeout counter
			timeoutCounter = new Timer();
			timeoutCounter.schedule(new TimerTask()
			{
				@Override
				public void run()
				{	//time's up!
					controller.timeout(field);
				}
			}, ((Timeoutable) field).getTimeoutS() * 1000 /*ms*/);
		}
		//else: do nothing
	}

	@Override
	public View getView()
	{
		return this;
	}
	
}
