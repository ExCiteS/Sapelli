package uk.ac.ucl.excites.sapelli.packager.io;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.StandardWatchEventKinds;
import java.nio.file.WatchEvent;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;

import lombok.extern.slf4j.Slf4j;
import uk.ac.ucl.excites.sapelli.packager.sapelli.ProjectChecker;

/**
 * A Folder Watcher that monitors for file changes in a Sapelli project directory
 * <p>
 * Created by Michalis on 28/06/2017.
 */
@Slf4j
public class ProjectWatcher
{
	private volatile Thread watchingThread;
	private ProjectListener listener;
	private ProjectChecker projectChecker;
	private boolean isWatching = false;

	public ProjectWatcher(ProjectChecker projectChecker, ProjectListener listener)
	{
		this.projectChecker = projectChecker;
		this.listener = listener;

		watchSapelliDirectory();
	}

	private void watchSapelliDirectory()
	{
		watchingThread = new Thread(() ->
		{
			// The WatchService
			WatchService fileWatcher = null;

			try
			{
				// Get Sapelli Project Dir to Monitor
				final Path directory = projectChecker.getSapelliProjectDir().toPath();

				// Create a WatchService and register the directory
				fileWatcher = FileSystems.getDefault().newWatchService();
				final WatchKey key = directory.register(fileWatcher,
				  StandardWatchEventKinds.ENTRY_CREATE,
				  StandardWatchEventKinds.ENTRY_DELETE,
				  StandardWatchEventKinds.ENTRY_MODIFY);

				log.info("Watch '{}' for file changes.", directory);

				// Start watching
				isWatching = true;
				while(isWatching)
				{
					// Prevent receiving two separate ENTRY_MODIFY events: file modified
					// and timestamp updated. Instead, receive one ENTRY_MODIFY event
					// with two counts.
					try
					{
						Thread.sleep(100);
					}
					catch(InterruptedException ignored)
					{
					}

					for(WatchEvent<?> event : key.pollEvents())
					{
						// Get event type
						WatchEvent.Kind<?> kind = event.kind();

						// Get file name
						@SuppressWarnings("unchecked")
						WatchEvent<Path> watchEventPath = (WatchEvent<Path>) event;
						Path fileName = watchEventPath.context();

						// Debug
						log.info("{}: {} ", kind, fileName);

						// If we have any kind of file modification
						if(kind == StandardWatchEventKinds.ENTRY_CREATE || kind == StandardWatchEventKinds.ENTRY_MODIFY || kind == StandardWatchEventKinds.ENTRY_DELETE)
						{
							// TODO: 28/06/2017 Check files
							listener.onFileChanged(fileName);
						}
					}

					// IMPORTANT: The key must be reset after processed
					if(!key.reset())
						break;
				}

			}
			catch(IOException e)
			{

				// Try to close the WatchService
				try
				{
					if(fileWatcher != null)
						fileWatcher.close();
				}
				catch(IOException ignored)
				{
				}

				log.error("Error while trying to monitor '{}' for file changes.", projectChecker.getSapelliProjectDir());
				log.error("The error was: ", e);
			}
		});

		// Start Watching
		watchingThread.start();
	}

	public void stopWatching()
	{
		isWatching = false;
		if(watchingThread != null)
			watchingThread.interrupt();
	}
}
