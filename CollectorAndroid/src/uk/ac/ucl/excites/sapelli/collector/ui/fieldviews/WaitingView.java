package uk.ac.ucl.excites.sapelli.collector.ui.fieldviews;

import java.util.Timer;
import java.util.TimerTask;

import uk.ac.ucl.excites.sapelli.collector.control.CollectorController;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.ui.FieldUI;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.util.Timeoutable;
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

	private final CollectorController controller;
	private final Field field;
	protected Timer timeoutCounter = null;
	
	
	public WaitingView(Context context, CollectorController controller, Field field)
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
