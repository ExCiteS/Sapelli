package uk.ac.ucl.excites.sapelli.sender;

import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.sender.util.SapelliAlarmManager;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

public class BootReceiver extends BroadcastReceiver
{
	@Override
	public void onReceive(Context context, Intent intent)
	{
		if(intent.getAction().equals("android.intent.action.BOOT_COMPLETED"))
		{
			// Check if projects require data transmission and set up alarms for the DataSenderService
			ProjectStore projectStore = null;
			try
			{
				// Get ProjectStore instance:
				projectStore = CollectorApp.GetProjectStore(context);

				// For each of the projects that has sending enabled, set an Alarm
				for(Project p : projectStore.retrieveProjects())
					// TODO if (p.isSending())
					// TODO interval should be saved in project -> p.getSendingInterval()
					SapelliAlarmManager.setAlarm(context, 60 * 1000, p.getID());
			}
			catch(Exception e)
			{
				// TODO
			}
			finally
			{
				if(projectStore != null)
					projectStore.finalise();
			}
		}
	}
}