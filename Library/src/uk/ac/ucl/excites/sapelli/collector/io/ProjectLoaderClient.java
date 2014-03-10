/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.io;

import uk.ac.ucl.excites.sapelli.collector.model.Project;

/**
 * Callbacks for ProjectLoader
 * 
 * @author mstevens
 */
public interface ProjectLoaderClient
{

	public boolean isDuplicateProject(Project loadProject);
		
}
