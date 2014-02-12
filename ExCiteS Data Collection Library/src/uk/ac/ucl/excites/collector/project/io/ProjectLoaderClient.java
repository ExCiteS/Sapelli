/**
 * 
 */
package uk.ac.ucl.excites.collector.project.io;

import uk.ac.ucl.excites.collector.project.model.Project;

/**
 * Callbacks for ProjectLoader
 * 
 * @author mstevens
 */
public interface ProjectLoaderClient
{

	public boolean isDuplicateProject(Project loadProject);
		
}
