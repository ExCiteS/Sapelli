package uk.ac.ucl.excites.relay;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

import uk.ac.ucl.excites.relay.sms.SmsDatabaseSQLite;
import uk.ac.ucl.excites.relay.util.Debug;
import uk.ac.ucl.excites.relay.util.UIHelper;
import uk.ac.ucl.excites.relay.util.Utilities;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.TextView;
import android.widget.Toast;

/**
 * Main Activity that shows the stats
 * 
 * @author Michalis Vitos
 * 
 */
public class BackgroundActivity extends Activity
{
	public static final String SERVICE_PACKAGE_NAME = BackgroundService.class.getName();
	private static SmsDatabaseSQLite dao;

	private Context mContext;

	// UI
	private UIHelper UI;
	private TextView serviceRunning;
	private TextView totalSmsReceived;
	private TextView totalSmsSent;
	private TextView lastSmsReceived;
	private TextView lastSmsSent;

	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		this.mContext = this;
		dao = new SmsDatabaseSQLite(this);

		// Check the preferences for a server address
		if(Preferences.getServerAddress(this).isEmpty())
		{
			Preferences.printPreferences(this);
			Toast t = Toast.makeText(this, "Please set up a Url server address.", Toast.LENGTH_LONG);
			t.show();

			Intent settingsActivity = new Intent(this, Preferences.class);
			startActivity(settingsActivity);
		}

		// UI Set up
		setContentView(R.layout.activity_background);
		serviceRunning = (TextView) findViewById(R.id.service_running);
		totalSmsReceived = (TextView) findViewById(R.id.total_sms_received);
		totalSmsSent = (TextView) findViewById(R.id.total_sms_sent);
		lastSmsReceived = (TextView) findViewById(R.id.last_sms_received);
		lastSmsSent = (TextView) findViewById(R.id.last_sms_sent);

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

		UI = new UIHelper(new Runnable()
		{
			@Override
			public void run()
			{
				// Get the Dates
				SimpleDateFormat dateFormat = new SimpleDateFormat("KK:mm:ss dd-MM-yyyy", Locale.ENGLISH);
				String lastReceived = (dao.getLastReceived() != 0) ? dateFormat.format(new Date(dao.getLastReceived())) : "-";
				String lastSent = (dao.getLastSent() != 0) ? dateFormat.format(new Date(dao.getLastSent())) : "-";

				// Set the UI
				totalSmsReceived.setText(Integer.toString(dao.getTotal()));
				totalSmsSent.setText(Integer.toString(dao.getSent()));
				lastSmsReceived.setText(lastReceived);
				lastSmsSent.setText(lastSent);
			}
		});
	}

	@Override
	protected void onResume()
	{
		super.onResume();

		// Start updates
		UI.startUpdates();

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
	protected void onPause()
	{
		super.onPause();

		// Stop updates
		UI.stopUpdates();
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
		case R.id.menu_log:

			Intent logIntent = new Intent(mContext, LogActivity.class);
			startActivity(logIntent);

			break;
		case R.id.menu_backup:

			SmsDatabaseSQLite.copyDBtoSD(this);

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
