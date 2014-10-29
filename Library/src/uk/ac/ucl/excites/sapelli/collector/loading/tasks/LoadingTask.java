/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.loading.tasks;

/**
 * @author mstevens
 *
 */
public interface LoadingTask
{

	public void execute(LoadingTaskExecutor executor) throws Exception;
	
}
