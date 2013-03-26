package uk.ac.ucl.excites.relay;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.TextView;

public class BackgroundActivity extends Activity
{
	public static final String SERVICE_PACKAGE_NAME = BackgroundService.class.getName();
	private Context mContext;
	private TextView serviceRunning;

	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_background);

		this.mContext = this;
		serviceRunning = (TextView) findViewById(R.id.service_running);

		// Get out of the AirplaneMode
		if(Utilities.inAirplaneMode(mContext))
			Utilities.toggleAirplaneMode(mContext);

		// Set the message
		if(Utilities.isMyServiceRunning(mContext, SERVICE_PACKAGE_NAME))
		{
			serviceRunning.setText(getResources().getString(R.string.service_is_running));
			Debug.d("Background.onStart(): Message Set.");
		}
		else
		{
			serviceRunning.setText(getResources().getString(R.string.service_is_not_running));
			// Call the Service
			Intent mIntent = new Intent(mContext, BackgroundService.class);
			startService(mIntent);
		}
	}

	@Override
	protected void onResume()
	{
		super.onResume();

		if(Utilities.isMyServiceRunning(mContext, SERVICE_PACKAGE_NAME))
		{
			serviceRunning.setText(getResources().getString(R.string.service_is_running));
			// if (Constants.DEBUG_LOG)
			// Log.i(Constants.TAG, "Background.onResume(): Message Set.");
		}
		else
		{
			serviceRunning.setText(getResources().getString(R.string.service_is_not_running));
		}
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu)
	{
		getMenuInflater().inflate(R.menu.menu_main, menu);
		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item)
	{
		switch(item.getItemId())
		{
		case R.id.menu_settings:

			Intent settingsActivity = new Intent(mContext, Preferences.class);
			startActivity(settingsActivity);

			break;
		case R.id.menu_about:

			Intent aboutActivity = new Intent(mContext, About.class);
			startActivity(aboutActivity);

			break;
		case R.id.menu_exit:
			// Terminate the Service
			while(Utilities.isMyServiceRunning(mContext, SERVICE_PACKAGE_NAME))
			{
				// Terminate the service
				Intent mIntent = new Intent(mContext, BackgroundService.class);
				if(mContext.stopService(mIntent))
				{
					// if (Constants.DEBUG_LOG)
					// Log.i(Constants.TAG,
					// "Background.onCreate(): Service Stoped.");
				}
				else
				{
					// if (Constants.DEBUG_LOG)
					// Log.i(Constants.TAG,
					// "Background.onCreate(): Cannot Stop the Service.");
				}
			}
			finish();
			break;
		}

		return true;
	}
}
