package uk.ac.ucl.excites.sapelli.sender.util;

import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.shared.db.StoreClient;
import uk.ac.ucl.excites.sapelli.util.Debug;
import android.app.IntentService;
import android.content.Intent;

/**
 * Simple Service for scheduling alarms for project that need transmission.
 * 
 * @author Michalis Vitos
 *
 */
public class AlarmScheduler extends IntentService implements StoreClient
{
	private ProjectStore projectStore;

	/**
	 * A constructor is required, and must call the super IntentService(String) constructor with a name for the worker thread.
	 */
	public AlarmScheduler()
	{
		super("AlarmScheduler");
	}

	/**
	 * The IntentService calls this method from the default worker thread with the intent that started the service. When this method returns, IntentService
	 * stops the service, as appropriate.
	 */
	@Override
	protected void onHandleIntent(Intent intent)
	{
		// Check if projects require data transmission and set up alarms for the DataSenderService
		projectStore = null;
		try
		{
			// Get ProjectStore instance:
			projectStore = ((CollectorApp) getApplication()).getProjectStore(this);

			// Set an Alarm, for each of the projects that has sending enabled
			for(Project p : projectStore.retrieveProjects())
			{
				// TODO if (p.isSending())
				// TODO interval should be saved in project -> p.getSendingInterval()
				SapelliAlarmManager.setAlarm(this, 60 * 1000, p.getID());
				Debug.d("Projects: " + p);
			}
		}
		catch(Exception e)
		{
			// TODO
		}
	}

	@Override
	public void onDestroy()
	{
		super.onDestroy();
		((CollectorApp) getApplication()).discardStoreUsage(projectStore, this);
	}
}
