package uk.ac.ucl.excites.sapelli.collector.ui.fields;

import java.util.Timer;
import java.util.TimerTask;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.fields.LocationField;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorView;
import uk.ac.ucl.excites.sapelli.shared.util.Timeoutable;
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
public class AndroidLocationUI extends LocationUI<View>
{

	private Button pageView;
	private LinearLayout waitView;
	private Timer timeoutCounter = null;
	
	public AndroidLocationUI(LocationField field, Controller controller, CollectorUI<View> collectorUI)
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
	public View getPlatformView(boolean onPage, CollectorRecord record)
	{
		if(onPage)
		{
			if(pageView == null)
			{
				pageView = new Button(((CollectorView) collectorUI).getContext());
				pageView.setText(field.getLabel());
				pageView.setOnClickListener(new OnClickListener()
				{
					@Override
					public void onClick(View v)
					{
						controller.goToFromPage(field);
					}
				});
				// TODO add spinner on button (when startWithForm or startWithPage), make change it for a "got location" icon when location is obtained
				// TODO show "got location" icon when already has location
			}
			return pageView;
		}
		else
		{
			if(waitView == null)
			{
				Context context = ((CollectorView) collectorUI).getContext();
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
					((CollectorView) collectorUI).getActivity().runOnUiThread(new Runnable()
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
