package uk.ac.ucl.excites.relay.util;

import android.os.Handler;

/**
 * Class for updating the UI
 * 
 * @author Michalis Vitos
 * 
 */
public class UIHelper
{
	private Handler handler = new Handler();
	private Runnable status;

	private int UPDATE_INTERVAL = 2000; // 2 seconds

	public UIHelper(final Runnable updater)
	{
		status = new Runnable()
		{
			@Override
			public void run()
			{
				// Run the passed runnable
				updater.run();
				// Re-run it after the update interval
				handler.postDelayed(this, UPDATE_INTERVAL);
			}
		};
	}

	public UIHelper(Runnable updater, int interval)
	{
		this(updater);
		UPDATE_INTERVAL = interval;
	}

	public void startUpdates()
	{
		status.run();
	}

	public void stopUpdates()
	{
		handler.removeCallbacks(status);
	}
}
