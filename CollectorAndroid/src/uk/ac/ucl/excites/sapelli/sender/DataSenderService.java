package uk.ac.ucl.excites.sapelli.sender;

import uk.ac.ucl.excites.sapelli.sender.util.SapelliAlarmManager;
import uk.ac.ucl.excites.sapelli.util.Debug;
import android.app.Service;
import android.content.Intent;
import android.os.Bundle;
import android.os.IBinder;

/**
 * @author Michalis Vitos
 */
public class DataSenderService extends Service
{
	@Override
	public int onStartCommand(Intent intent, int flags, int startId)
	{
		Bundle extras = intent.getExtras();
		final long projectId = extras.getInt(SapelliAlarmManager.PROJECT_ID);
		Debug.d("Call to service: " + startId + " for project: " + projectId);

		return Service.START_NOT_STICKY;
	}

	@Override
	public void onDestroy()
	{
		Debug.d("Service has been killed!");
	}

	@Override
	public IBinder onBind(Intent intent)
	{
		// TODO Auto-generated method stub
		return null;
	}
}