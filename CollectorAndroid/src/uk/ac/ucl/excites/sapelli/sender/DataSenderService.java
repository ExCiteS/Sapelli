package uk.ac.ucl.excites.sapelli.sender;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;

/**
 * A service that checks about network connectivity and tries to send the data Also the service tries to upload file attachments to Dropbox
 * 
 * @author Michalis Vitos
 * 
 */
public class DataSenderService extends Service
{

	@Override
	public int onStartCommand(Intent intent, int flags, int startId)
	{
		return startId;
	}

	@Override
	public void onDestroy()
	{
	}

	@Override
	public IBinder onBind(Intent intent)
	{
		// TODO Auto-generated method stub
		return null;
	}
}