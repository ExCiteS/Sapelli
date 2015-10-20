/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.collector.db;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.db.exceptions.ProjectAlreadyStoredException;
import uk.ac.ucl.excites.sapelli.collector.db.exceptions.ProjectDuplicateException;
import uk.ac.ucl.excites.sapelli.collector.db.exceptions.ProjectIdentificationClashException;
import uk.ac.ucl.excites.sapelli.collector.db.exceptions.ProjectSignatureClashException;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.model.ProjectDescriptor;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Relationship;
import uk.ac.ucl.excites.sapelli.collector.transmission.SendingSchedule;
import uk.ac.ucl.excites.sapelli.shared.db.Store;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.storage.model.RecordReference;
import uk.ac.ucl.excites.sapelli.transmission.db.TransmissionStore;

/**
 * Abstract super class for Project storage back-ends
 * 
 * @author mstevens
 */
public abstract class ProjectStore extends Store
{
	
	static public final String DATABASE_NAME_SUFFIX = "_Projects";
	static public final String BACKUP_SUFFIX = "_Backup_"; // to be followed by a timestamp
	
	/**
	 * Stores the given project, provided it doesn't clash with a previously stored one.<br/>
	 * A clash means that there is a previously stored project which is different *but* has the same signature (name+[variant]+version) or identification (id+fingerprint).
	 * If the exact same project is already stored nothing happens.
	 * 
	 * @param project
	 * @return the stored project
	 * @throws ProjectSignatureClashException when there is a previously stored project which is different but has the same signature (name+[variant]+version)
	 * @throws ProjectIdentificationClashException when there is a previously stored project which is different but has the same identification (id+fingerprint)
	 */
	public Project add(Project project) throws ProjectSignatureClashException, ProjectIdentificationClashException
	{
		if(project == null)
			throw new NullPointerException("Project is null");
		if(!isStored(project, true))
			// Go ahead with storing project:
			doAdd(project);
		// Return if successful (can also mean the exact same project was already stored):
		return project;
	}
	
	/**
	 * Checks if the given project, or a similar one, is already in the database.<br/>
	 * If *exact* same project is already stored this method returns {@code true}.
	 * If a different project with the same identification (id+fingerprint), or the same signature (name+[variant]+version)
	 * is already stored then this method will respectively throw a {@link ProjectIdentificationClashException} or a {@link ProjectSignatureClashException} exception.
	 * If no equal or similar project is currently stored the method returns {@code false}
	 * 
	 * @param project
	 * @param queryOnSignature whether or to query based on signature
	 * @return whether or not exactly the same project is already stored
	 * @throws ProjectSignatureClashException when there is a previously stored project which is different but has the same signature (name+[variant]+version)
	 * @throws ProjectIdentificationClashException when there is a previously stored project which is different but has the same identification (id+fingerprint)
	 */
	protected boolean isStored(Project project, boolean queryOnSignature) throws ProjectSignatureClashException, ProjectIdentificationClashException
	{
		// Check for identification (id+fingerprint) clash:
		Project dupe = retrieveProject(project.getID(), project.getFingerPrint());
		if(queryOnSignature && dupe == null)
			// Check for signature (name+[variant]+version) clash:
			dupe = retrieveProject(project.getName(), project.getVariant(), project.getVersion());
		
		// Handle duplicate...
		if(dupe != null)
		{
			if(!project.equals(dupe))
			{
				if(project.equalSignature(dupe))
					throw new ProjectSignatureClashException(dupe);
				else
					throw new ProjectIdentificationClashException(dupe, true);
			}
			else
				return true; // duplicate but equal --> this project is already in the store
		}
		return false;
	}
	
	/**
	 * Checks if the given project is similar or equal to a previously stored one
	 * 
	 * @param project
	 * @throws ProjectSignatureClashException when there is a previously stored project which is different but has the same signature (name+[variant]+version)
	 * @throws ProjectIdentificationClashException when there is a previously stored project which is different but has the same identification (id+fingerprint)
	 * @throws ProjectAlreadyStoredException when the exact same project is already stored
	 */
	public void duplicateCheck(Project project) throws ProjectSignatureClashException, ProjectIdentificationClashException, ProjectAlreadyStoredException
	{
		if(isStored(project, true))
			throw new ProjectAlreadyStoredException(project);
	}
	
	/**
	 * Stores the project.
	 * 
	 * @param project
	 * @throws ProjectDuplicateException some implementation may throw this in case of a identification or signature clash
	 */
	protected abstract void doAdd(Project project) throws ProjectDuplicateException;
	
	/**
	 * Retrieves all projects
	 * 
	 * @return
	 */
	public abstract List<Project> retrieveProjects();
	
	/**
	 * Retrieves all projects as ProjectDescriptor or Project instances.
	 * If the ProjectStore implements a caching mechanism than any cached Project objects will be returned as such (rather than as ProjectDescriptors).
	 * 
	 * @return
	 */
	public abstract <P extends ProjectDescriptor> List<P> retrieveProjectsOrDescriptors();
	
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
	 * Retrieves specific Project, identified by id and fingerprint
	 * 
	 * @param projectID
	 * @param projectFingerPrint
	 * @return null if no such project was found
	 */
	public abstract Project retrieveProject(int projectID, int projectFingerPrint);
	
	/**
	 * Retrieves specific a project, returned as a ProjectDescriptor or a Project instance.
	 * If the ProjectStore implements a caching mechanism than any cached Project objects will be returned as such (rather than as ProjectDescriptors).
	 * 
	 * @param projectID
	 * @param projectFingerPrint
	 * @return null if no such project was found
	 */
	public abstract ProjectDescriptor retrieveProjectOrDescriptor(int projectID, int projectFingerPrint);
	
	/**
	 * Retrieves specific Project, identified by ProjectDescriptor
	 * 
	 * @param descriptor
	 * @return
	 */
	public abstract Project retrieveProject(ProjectDescriptor descriptor);
	
	/**
	 * For backwards compatibility only
	 * 
	 * @param id
	 * @param version
	 * @return
	 */
	public abstract Project retrieveV1Project(int schemaID, int schemaVersion);
	
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
	 * @param project
	 */
	public abstract void delete(Project project);
	
	public abstract void storeSendSchedule(SendingSchedule schedule, TransmissionStore transmissionStore);
	
	public abstract SendingSchedule retrieveSendScheduleForProject(Project project, TransmissionStore transmissionStore) throws DBException;
	
	public abstract void deleteSendSchedule(SendingSchedule schedule);

	/**
	 * Delete specific Project, identified by ProjectDescriptor
	 * 
	 * @param projectDescriptor
	 */
	public abstract void delete(ProjectDescriptor projectDescriptor);
	
	public abstract void storeHeldForeignKey(Relationship relationship, RecordReference foreignKey);
	
	public abstract RecordReference retrieveHeldForeignKey(Relationship relationship);
	
	public abstract void deleteHeldForeignKey(Relationship relationship);
	
	/**
	 * Must serialise the given Project instance and write the result to the given OutputStream.
	 * The serialised representation must be self-contained (i.e. it must fully describe the Project) and be understood by {@link #deserialise(InputStream)}.
	 * 
	 * The given project is not necessarily already be stored in the ProjectStore, and if it is not, it won't be the method returns either.
	 * The method implementation may use, but should *not* rely, on querying the ProjectStore itself.
	 *  
	 * @param project the Project instance to serialise
	 * @param out the OutputStream to write to
	 * @throws IOException in case an I/O error occurs
	 */
	public abstract void serialise(Project project, OutputStream out) throws IOException;
	
	/**
	 * Must read a serialised Project (as produced by {@link #serialise(Project, OutputStream)}) from the given InputStream and deserialse it back to a Project instance.
	 * May use but should not rely on querying the ProjectStore itself.
	 * 
	 * The given project is not necessarily already be stored in the ProjectStore, and if it is not, it won't be the method returns either.
	 * The method implementation may use, but should *not* rely, on querying the ProjectStore itself.
	 * 
	 * @param in the InputStream to read from
	 * @return the deserialised Project instance
	 * @throws IOException in case an I/O error occurs
	 */
	public abstract Project deserialise(InputStream in) throws IOException;

}