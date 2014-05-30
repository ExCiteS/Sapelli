package uk.ac.ucl.excites.sapelli.sender;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import uk.ac.ucl.excites.sapelli.sender.util.SapelliAlarmManager;
import uk.ac.ucl.excites.sapelli.util.Debug;
import android.app.Service;
import android.content.Intent;
import android.os.IBinder;

/**
 * @author Michalis Vitos
 */
public class DataSenderService extends Service
{
	private BlockingQueue<Integer> projectQueue = new ArrayBlockingQueue<Integer>(1024);
	// Use a single Thread and Send the Projects sequential
	private ExecutorService projectExecutor = Executors.newSingleThreadExecutor();
	private Runnable projectTask = new ProjectSendingTask(projectQueue);

	@Override
	public synchronized int onStartCommand(Intent intent, int flags, int startId)
	{
		// Add Project ID to the Queue
		addProject(intent.getExtras().getInt(SapelliAlarmManager.PROJECT_ID));

		// Run Projects
		runProjects();

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
		return null;
	}

	/**
	 * Add the projectId to the blocking queue
	 * 
	 * @param projectId
	 */
	private void addProject(int projectId)
	{
		try
		{
			projectQueue.put(projectId);
		}
		catch(InterruptedException e)
		{
			Debug.e(e);
		}
	}

	/**
	 * Check if the Thread is already running and execute the projectTask
	 */
	private void runProjects()
	{
		if(!projectExecutor.isTerminated())
			projectExecutor.execute(projectTask);
	}

	/**
	 * A Runnable Class that is responsible for Transmitting the data for each of the projects in its BlockingQueue
	 * 
	 * @author Michalis Vitos
	 * 
	 */
	public class ProjectSendingTask implements Runnable
	{
		private BlockingQueue<Integer> queue = null;

		ProjectSendingTask(BlockingQueue<Integer> queue)
		{
			this.queue = queue;
		}

		@Override
		public void run()
		{
			while(!queue.isEmpty())
			{
				// Print the queue
				Debug.d("queue: " + queue.toString());

				try
				{
					Debug.d("Project " + queue.take() + " is running and it takes 20 seconds");
					Thread.sleep(20 * 1000);
				}
				catch(InterruptedException e)
				{
					e.printStackTrace();
				}
			}

			// Stop the Android Service
			stopSelf();
		}
	}
}