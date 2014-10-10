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

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.io.ProjectLoader;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.model.fields.Relationship;
import uk.ac.ucl.excites.sapelli.collector.util.DuplicateException;
import uk.ac.ucl.excites.sapelli.collector.xml.ProjectParser;
import uk.ac.ucl.excites.sapelli.storage.model.RecordReference;
import android.content.Context;
import android.content.SharedPreferences;
import android.util.Log;

/**
 * Project storage back-end using Android SharedPreferences, a cache and re-parsing of project XML files
 * 
 * @author Michalis Vitos, mstevens
 */
/**
 * @author mstevens
 *
 */
public class PrefProjectStore extends ProjectStore
{
	
	// Statics----------------------------------------------
	static protected final String TAG = "DB4OPrefDataAccess";
	private static final String PREFERENCES_NAME = "PROJECT_STORAGE";
	private static final String PREF_KEY_SEPARATOR = "_";
	private static final String PREF_PROJECT_PATH_PREFIX = "PROJECT";
	private static final String PREF_PROJECT_PATH_POSTFIX = "PATH";
	private static final String HELD_FOREIGN_KEY_PREFIX = "RELATIONSHIP_";
	private static final String HELD_FOREIGN_KEY_POSTFIX = "_HELD_FOREIGN_KEY";

	// Dynamics---------------------------------------------
	private Context context;
	private SharedPreferences preferences;
	private Set<Project> projectCache;

	public PrefProjectStore(Context context)
	{
		this(context, "");
	}
	
	public PrefProjectStore(Context context, String prefix)
	{
		this.context = context;
		this.preferences = this.context.getSharedPreferences(prefix + PREFERENCES_NAME, Context.MODE_PRIVATE);
	}
	
	/**
	 * @param project
	 */
	@Override
	public void store(Project project) throws DuplicateException
	{
		// Check for project duplicates:
		if(retrieveProject(project.getName(), project.getVariant(), project.getVersion()) != null)
			throw new DuplicateException("There is already a project with signature \"" + project.toString(false) + "\". Either delete the existing one or change the version of the new one.");
		// Check for id & finger print collision (very unlikely, but highly problematic):
		Project dupe = retrieveProject(project.getID(), project.getFingerPrint());
		if(dupe != null && !project.equals(dupe))
			throw new DuplicateException("Project id & finger print collision!");
		// Store in prefs:
		storeProjectPathPrefKey(project);
		// Store in cache:
		cacheProject(project);
	}
	
	/**
	 * Retrieves all projects
	 * 
	 * @return
	 */
	@Override
	public List<Project> retrieveProjects()
	{
		updateProjectCache(); // will also call initProjectCache()
		return new ArrayList<Project>(projectCache);
	}
	
	/**
	 * Retrieves specific Project
	 * 
	 * @return the project object or null if project was not found
	 */
	@Override
	public Project retrieveProject(final String name, final String variant, final String version)
	{
		updateProjectCache(); // will also call initProjectCache()
		for(Project project : projectCache)
		{
			if( project.getName().equalsIgnoreCase(name)
				&& (variant != null ? variant.equals(project.getVariant()) : true)
			    && project.getVersion().equalsIgnoreCase(version))
				return project;
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.database.IDataAccess#retrieveV1Project(int, int)
	 */
	@Override
	public Project retrieveV1Project(final int schemaID, final int schemaVersion)
	{
		updateProjectCache(); // will also call initProjectCache()
		for(Project project : projectCache)
			if(project.isV1xProject() && project.getID() == schemaID && project.getV1XSchemaVersion() == schemaVersion)
				return project;
		return null;
	}
	
	/**
	 * Retrieves specific Project
	 * 
	 * @return null if project was not found
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#retrieveProject(int, int)
	 */
	@Override
	public Project retrieveProject(int projectID, int projectFingerPrint)
	{
		// Get project from cache if it is there ...
		Project cachedProject = getCachedProject(projectID, projectFingerPrint);
		if(cachedProject != null)
			return cachedProject;
		
		// ... parse the project if not ...
		String folderPath = preferences.getString(getProjectPathPrefKey(projectID, projectFingerPrint), null);
		if(folderPath != null)
		{
			Project p = parseProject(folderPath);
			if(p != null)
			{
				cacheProject(p); // cache the project (the cache will be initialised if needed)
				return p;
			}
			else
				removeProjectPathPrefKey(projectID, projectFingerPrint); // we were unable to parse a project at the path, so remove it from the preferences 
		}
		
		// Project not found:
		return null;
	}
	
	@Override
	public Project retrieveProject(String name, String version)
	{
		return retrieveProject(name, null, version);
	}

	/**
	 * Delete specific project
	 * 
	 * @return
	 */
	@Override
	public void delete(Project project)
	{
		// Try to remove from cache:
		if(projectCache.remove(project))
			// if it was in cache, remove from prefs as well:
			removeProjectPathPrefKey(project);
	}
	
	private void initProjectCache()
	{
		if(projectCache == null)
			projectCache = new HashSet<Project>();
	}
	
	private void updateProjectCache()
	{
		// Ensure we have a cache at all:
		initProjectCache();
		// Loop through project path preference keys:
		for(Map.Entry<String, ?> entry : preferences.getAll().entrySet())
			if(entry.getKey().startsWith(PREF_PROJECT_PATH_PREFIX) && entry.getKey().endsWith(PREF_PROJECT_PATH_POSTFIX))
			{
				int projectID = getProjectID(entry.getKey());
				int projectFingerPrint = getProjectFingerPrint(entry.getKey());
				if(getCachedProject(projectID, projectFingerPrint) == null)
				{	// Parse the project if it is not already in the cache:
					Project p = parseProject(entry.getValue().toString());
					if(p != null)
					{
						if(p.getFingerPrint() != projectFingerPrint)
						{
							Log.w(TAG, "XML finger print of project " + p.toString() + " has changed, possibly the " + ProjectLoader.PROJECT_FILE + " file (located in " + entry.getValue().toString() + ") was manually edited!");
							// Remove old pref key:
							removeProjectPathPrefKey(projectID, projectFingerPrint);
							// Add new pref key:
							storeProjectPathPrefKey(p);
						}
						// Cache the project object:
						cacheProject(p);
					}
				}
			}
	}
	
	private void cacheProject(Project project)
	{
		initProjectCache(); //will create an empty cache if we don't have one yet
		projectCache.add(project);
	}
	
	/**
	 * Gets project from cache if it is there, returns null otherwise
	 * 
	 * @param projectID
	 * @param projectFingerPrint
	 * @return
	 */
	private Project getCachedProject(int projectID, int projectFingerPrint)
	{
		if(projectCache != null)
			for(Project cachedProj : projectCache)
				if(cachedProj.getID() == projectID && cachedProj.getFingerPrint() == projectFingerPrint)
					return cachedProj;
		return null;
	}
	
	private void removeProjectPathPrefKey(Project project)
	{
		preferences.edit().remove(getProjectPathPrefKey(project)).commit();
	}
	
	private void removeProjectPathPrefKey(int projectID, int projectFingerPrint)
	{
		preferences.edit().remove(getProjectPathPrefKey(projectID, projectFingerPrint)).commit();
	}
	
	private void storeProjectPathPrefKey(Project project)
	{
		preferences.edit().putString(getProjectPathPrefKey(project), project.getProjectFolderPath()).commit();
	}
	
	private String getProjectPathPrefKey(Project project)
	{
		return getProjectPathPrefKey(project.getID(), project.getFingerPrint());
	}
	
	private String getProjectPathPrefKey(int projectID, int projectFingerPrint)
	{
		return 	PREF_PROJECT_PATH_PREFIX +
				PREF_KEY_SEPARATOR + projectID +
				PREF_KEY_SEPARATOR + projectFingerPrint +
				PREF_KEY_SEPARATOR + PREF_PROJECT_PATH_POSTFIX;
	}
	
	private int getProjectID(String projectPathPrefKey)
	{
		return Integer.parseInt(projectPathPrefKey.split("\\" + PREF_KEY_SEPARATOR)[1]);
	}
	
	private int getProjectFingerPrint(String projectPathPrefKey)
	{
		return Integer.parseInt(projectPathPrefKey.split("\\" + PREF_KEY_SEPARATOR)[2]);
	}
	
	private Project parseProject(String folderPath)
	{
		File xmlFile = new File(folderPath.toString() + ProjectLoader.PROJECT_FILE);
		try
		{
			final CollectorApp app = (CollectorApp) context.getApplicationContext();
			ProjectParser parser = new ProjectParser(app.getSapelliFolderPath(), false);
			return parser.parseProject(xmlFile);
		}
		catch(Exception e)
		{
			e.printStackTrace();
		}
		return null;
	}

	private String getHeldForeignKeyPrefKey(Relationship relationship)
	{
		return HELD_FOREIGN_KEY_PREFIX + "(" + getProjectPathPrefKey(relationship.getForm().getProject()) + ")_" + relationship.getForm().getPosition() + "_" + relationship.getID() + HELD_FOREIGN_KEY_POSTFIX;
	}
	
	@Override
	public List<Project> retrieveProjectVersions(int projectID)
	{
		updateProjectCache(); // will also call initProjectCache()
		List<Project> projectVersions = new ArrayList<Project>();
		for(Project project : projectCache)
			if(project.getID() == projectID)
				projectVersions.add(project);
		return projectVersions;
	}
	
	@Override
	public void storeHeldForeignKey(Relationship relationship, RecordReference foreignKey)
	{
		if(!relationship.isHoldForeignRecord())
			throw new IllegalArgumentException("This relationship is not allowed to hold on to foreign records");
		preferences.edit().putString(getHeldForeignKeyPrefKey(relationship), foreignKey.serialise()).commit();
	}

	@Override
	public RecordReference retrieveHeldForeignKey(Relationship relationship)
	{
		if(!relationship.isHoldForeignRecord())
			throw new IllegalArgumentException("This relationship is not allowed to hold on to foreign records");
		String prefKey = getHeldForeignKeyPrefKey(relationship);
		String serialisedForeignKey = preferences.getString(prefKey, null);
		if(serialisedForeignKey != null)
			try
			{
				return relationship.getRelatedForm().getSchema().createRecordReference(serialisedForeignKey);
			}
			catch(Exception e)
			{
				deleteHeldForeignKey(relationship);
			}
		return null;
	}
	
	@Override
	public void deleteHeldForeignKey(Relationship relationship)
	{
		// Don't check for isHoldForeignRecord() here!
		preferences.edit().remove(getHeldForeignKeyPrefKey(relationship)).commit();
	}
	
	@Override
	public void finalise()
	{
		// does nothing
	}
	
	@Override
	public void backup(File destinationFolder)
	{
		// TODO backup
	}

}
