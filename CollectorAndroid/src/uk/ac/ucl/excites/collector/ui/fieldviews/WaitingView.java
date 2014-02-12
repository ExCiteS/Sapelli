package uk.ac.ucl.excites.collector.ui.fieldviews;

import java.util.Timer;
import java.util.TimerTask;

import uk.ac.ucl.excites.collector.ProjectController;
import uk.ac.ucl.excites.collector.project.model.fields.Field;
import uk.ac.ucl.excites.collector.project.ui.FieldUI;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.util.Timeoutable;
import android.annotation.SuppressLint;
import android.content.Context;
import android.view.Gravity;
import android.widget.LinearLayout;
import android.widget.ProgressBar;

/**
 * @author Julia, mstevens
 *
 */
@SuppressLint("ViewConstructor")
public class WaitingView extends LinearLayout implements FieldUI
{

	private final ProjectController controller;
	private final Field field;
	protected Timer timeoutCounter = null;
	
	
	public WaitingView(Context context, ProjectController controller, Field field)
	{
		super(context);
		this.controller = controller;
		this.field = field;
		
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
	public Field getField()
	{
		return field;
	}

	@Override
	public void update(Record record)
	{
		if(field instanceof Timeoutable)
		{
			// Cancel previous counter:
			cancel();
			// Start timeout counter
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
	public boolean isValid(Record record)
	{
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void storeValue(Record record)
	{
		// TODO Auto-generated method stub
		
	}
	
}
