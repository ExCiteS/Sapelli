package uk.ac.excites.transmission.snd;

import uk.ac.excites.transmission.snd.R;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;

public class BackgroundActivity extends Activity {

	private final String SERVICE_PACKAGE_NAME = BackgroundService.class.getName();

	private Context mContext;
	private Button startButton;
	private Button stopButton;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_background);

		this.mContext = this;
		startButton = (Button) findViewById(R.id.startService);
		stopButton = (Button) findViewById(R.id.stopService);
		// Disable the Stop button
		stopButton.setEnabled(false);

		// Get in the AirplaneMode
		if (!Utilities.inAirplaneMode(mContext))
			Utilities.toggleAirplaneMode(mContext);

		// Start Button
		startButton.setOnClickListener(new View.OnClickListener() {

			public void onClick(View v) {
				// Call the Service
				Intent mIntent = new Intent(mContext, BackgroundService.class);
				startService(mIntent);

				// Disable the Start button and enable the Stop
				startButton.setEnabled(false);
				stopButton.setEnabled(true);
			}
		});

		// Stop Button
		stopButton.setOnClickListener(new View.OnClickListener() {

			public void onClick(View v) {

				// Terminate the Service
				while (Utilities.isMyServiceRunning(mContext, SERVICE_PACKAGE_NAME)) {
					// Terminate the service
					Intent mIntent = new Intent(mContext, BackgroundService.class);
					if (mContext.stopService(mIntent)) {
						if (Constants.DEBUG_LOG)
							Log.i(Constants.TAG, "Background.onCreate(): Service Stoped.");
					} else {
						if (Constants.DEBUG_LOG)
							Log.i(Constants.TAG, "Background.onCreate(): Cannot Stop the Service.");
					}
				}

				// Disable the Start button and enable the Stop
				startButton.setEnabled(true);
				stopButton.setEnabled(false);
			}
		});
	}

	@Override
	protected void onResume() {
		// TODO Auto-generated method stub
		super.onResume();

		if (Utilities.isMyServiceRunning(mContext, SERVICE_PACKAGE_NAME)) {
			startButton.setEnabled(false);
			stopButton.setEnabled(true);
			if (Constants.DEBUG_LOG)
				Log.i(Constants.TAG, "Background.onResume(): Buttons Set.");
		}
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		getMenuInflater().inflate(R.menu.menu_main, menu);
		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case R.id.menu_settings:
			
			Intent settingsActivity = new Intent(mContext, Preferences.class);
			startActivity(settingsActivity);

			break;
		}
		return true;
	}
}
