/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.project.io;

import uk.ac.ucl.excites.sapelli.collector.project.model.Project;

/**
 * Callbacks for ProjectLoader
 * 
 * @author mstevens
 */
public interface ProjectLoaderClient
{

	public boolean isDuplicateProject(Project loadProject);
		
}
