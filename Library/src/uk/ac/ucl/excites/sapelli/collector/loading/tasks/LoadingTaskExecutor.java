/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.loading.tasks;

import uk.ac.ucl.excites.sapelli.collector.loading.ProjectLoader;

/**
 * Interface for loading task executors, currently only implemented by {@link ProjectLoader} (and subclasses)
 * 
 * Add an execute() method here for each new class which implements {@link LoadingTask}.
 * 
 * @author mstevens
 */
public interface LoadingTaskExecutor
{

	public void execute(TTSSynthesisTask ttsTask);
	
}
