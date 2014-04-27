package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import java.util.Timer;
import java.util.TimerTask;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.control.FieldWithArguments;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LocationField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.shared.util.Timeoutable;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import android.content.Context;
import android.view.Gravity;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.ProgressBar;

/**
 * @author Julia, mstevens
 *
 */
public class AndroidLocationUI extends LocationUI<View, CollectorView>
{

	private Button pageView;
	private LinearLayout waitView;
	private Timer timeoutCounter = null;
	
	public AndroidLocationUI(LocationField field, Controller controller, CollectorView collectorUI)
	{
		super(field, controller, collectorUI);
	}

	@Override
	public void cancel()
	{
		if(timeoutCounter != null)
			timeoutCounter.cancel();
		//else: do nothing
	}
	
	@Override
	protected View getPlatformView(boolean onPage, Record record, boolean newRecord)
	{
		//TODO editable
		if(onPage)
		{
			if(pageView == null)
			{
				pageView = new Button(collectorUI.getContext());
				pageView.setText(field.getCaption());
				// TODO some kind of icon/image would be nice (an little flag or crosshairs?)
				pageView.setOnClickListener(new OnClickListener()
				{
					@Override
					public void onClick(View v)
					{
						controller.goTo(new FieldWithArguments(field), true); // force leave the page (PageUI#leave() will not be called!) to go to the field itself
					}
				});
				// TODO add spinner on button (when startWithForm or startWithPage), make change it for a "got location" icon when location is obtained
				// TODO show "got location" icon when already has location
			}
			return pageView;
		}
		else
		{
			// TODO show coordinates/accuracy to literate users (this will need a new XML attribute)
			if(waitView == null)
			{
				Context context = collectorUI.getContext();
				waitView = new LinearLayout(context);
				waitView.setGravity(Gravity.CENTER);
				waitView.addView(new ProgressBar(context, null, android.R.attr.progressBarStyleLarge));
			}
			
			// Cancel previous counter:
			cancel();
			
			// Start timeout counter
			timeoutCounter = new Timer();
			timeoutCounter.schedule(new TimerTask()
			{
				@Override
				public void run()
				{	//time's up!
					collectorUI.getActivity().runOnUiThread(new Runnable()
					{
						@Override
						public void run()
						{
							timeout();
						}
					});
				}
			}, ((Timeoutable) field).getTimeoutS() * 1000 /*ms*/);
			
			return waitView;
		}
		
	}

}
