package uk.ac.ucl.excites.sapelli.collector.db;

import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Relationship;
import uk.ac.ucl.excites.sapelli.collector.util.DuplicateException;
import uk.ac.ucl.excites.sapelli.shared.db.Store;
import uk.ac.ucl.excites.sapelli.storage.model.ForeignKey;

/**
 * Interface for Project storage back-ends
 * 
 * @author mstevens
 */
public abstract class ProjectStore implements Store
{

	/**
	 * For backwards compatibility only
	 * 
	 * @param id
	 * @param version
	 * @return
	 */
	public abstract Project retrieveV1Project(int schemaID, int schemaVersion);

	/**
	 * @param project
	 */
	public abstract void store(Project project) throws DuplicateException;

	/**
	 * Retrieves all projects
	 * 
	 * @return
	 */
	public abstract List<Project> retrieveProjects();

	/**
	 * Retrieves specific Project
	 * 
	 * @return null if project was not found
	 */
	public Project retrieveProject(final String name, final String version)
	{
		return retrieveProject(name, null, version);
	}

	/**
	 * Retrieves specific Project
	 * 
	 * @return null if project was not found
	 */
	public abstract Project retrieveProject(String name, String variant, String version);

	/**
	 * Retrieves specific Project, identified by id and hash
	 * 
	 * @param projectID
	 * @param projectHash
	 * @return null if no such project was found
	 */
	public abstract Project retrieveProject(int projectID, int projectHash);
	
	/**
	 * Retrieves all project versions/variants which share a given ID
	 * 
	 * @param projectID
	 * @return list of projects
	 */
	public abstract List<Project> retrieveProjectVersions(int projectID);

	/**
	 * Delete specific project
	 * 
	 * @return
	 */
	public abstract void delete(Project project);
	
	public abstract void storeHeldForeignKey(Relationship relationship, ForeignKey foreignKey);
	
	public abstract ForeignKey retrieveHeldForeignKey(Relationship relationship);
	
	public abstract void deleteHeldForeignKey(Relationship relationship);

}