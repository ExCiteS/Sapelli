package uk.ac.ucl.excites.sender;

import uk.ac.ucl.excites.collector.R;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.Button;

public class SenderBackgroundActivity extends Activity
{

	private final String SERVICE_PACKAGE_NAME = SenderBackgroundService.class.getName();

	private Context mContext;
	private Button startButton;
	private Button stopButton;
	private Button settingsButton;

	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_background);

		this.mContext = this;
		startButton = (Button) findViewById(R.id.startService);
		stopButton = (Button) findViewById(R.id.stopService);
		settingsButton = (Button) findViewById(R.id.settingsService);

		// Disable the Stop button
		stopButton.setEnabled(false);

		// Get in the AirplaneMode
		if(!Utilities.inAirplaneMode(mContext))
			Utilities.toggleAirplaneMode(mContext);

		// Start Button
		startButton.setOnClickListener(new View.OnClickListener()
		{

			public void onClick(View v)
			{
				// Call the Service
				Intent mIntent = new Intent(mContext, SenderBackgroundService.class);
				startService(mIntent);

				// Disable the Start button and enable the Stop
				startButton.setEnabled(false);
				stopButton.setEnabled(true);
			}
		});

		// Stop Button
		stopButton.setOnClickListener(new View.OnClickListener()
		{

			public void onClick(View v)
			{

				// Terminate the Service
				while(Utilities.isMyServiceRunning(mContext, SERVICE_PACKAGE_NAME))
				{
					// Terminate the service
					Intent mIntent = new Intent(mContext, SenderBackgroundService.class);
					if(mContext.stopService(mIntent))
					{
						if(Constants.DEBUG_LOG)
							Log.i(Constants.TAG, "Background.onCreate(): Service Stoped.");
					}
					else
					{
						if(Constants.DEBUG_LOG)
							Log.i(Constants.TAG, "Background.onCreate(): Cannot Stop the Service.");
					}
				}

				// Disable the Start button and enable the Stop
				startButton.setEnabled(true);
				stopButton.setEnabled(false);
			}
		});

		// Settings Button
		settingsButton.setOnClickListener(new View.OnClickListener()
		{

			public void onClick(View v)
			{
				// Start the Settings
				Intent settingsActivity = new Intent(mContext, SenderBackgroundPreferences.class);
				startActivity(settingsActivity);
			}
		});

	}

	@Override
	protected void onResume()
	{
		// TODO Auto-generated method stub
		super.onResume();

		if(Utilities.isMyServiceRunning(mContext, SERVICE_PACKAGE_NAME))
		{
			startButton.setEnabled(false);
			stopButton.setEnabled(true);
			if(Constants.DEBUG_LOG)
				Log.i(Constants.TAG, "Background.onResume(): Buttons Set.");
		}
	}
}
