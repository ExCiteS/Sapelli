/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.db;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import uk.ac.ucl.excites.sapelli.collector.CollectorApp;
import uk.ac.ucl.excites.sapelli.collector.io.ProjectLoader;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.util.DuplicateException;
import uk.ac.ucl.excites.sapelli.collector.xml.ProjectParser;
import android.content.Context;
import android.content.SharedPreferences;
import android.util.Log;

/**
 * Project storage back-end using Android SharedPreferences, a cache and re-parsing of project XML files
 * 
 * @author Michalis Vitos, mstevens
 */
public class PrefProjectStore extends ProjectStore
{
	
	// Statics----------------------------------------------
	static protected final String TAG = "DB4OPrefDataAccess";
	private static final String PREFERENCES_NAME = "PROJECT_PATH_STORAGE";
	private static final String PREF_PROJECT_PATH_PREFIX = "PROJECT_";
	private static final String PREF_PROJECT_PATH_POSTFIX = "_PATH";

	// Dynamics---------------------------------------------
	private Context context;
	private SharedPreferences preferences;
	private Map<Long,Project> projectCache;

	public PrefProjectStore(Context context)
	{
		this.context = context;
		this.preferences = this.context.getSharedPreferences(CollectorApp.getDemoPrefix() /*will be "" if not in demo mode*/ + PREFERENCES_NAME, Context.MODE_PRIVATE);
	}
	
	/**
	 * @param project
	 */
	@Override
	public void store(Project project) throws DuplicateException
	{
		// Check for project duplicates:
		if(retrieveProject(project.getName(), project.getVariant(), project.getVersion()) != null)
			throw new DuplicateException("There is already a project named \"" + project.getName() + "\", with version " + project.getVersion() + ". Either remove the existing one or increment the version of the new one.");
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
		return new ArrayList<Project>(retrieveProjectsColl());
	}
	
	private Collection<Project> retrieveProjectsColl()
	{
		updateProjectCache(); // will also call initProjectCache()
		return projectCache.values();
	}
	
	/**
	 * Retrieves specific Project
	 * 
	 * @return the project object or null if project was not found
	 */
	@Override
	public Project retrieveProject(final String name, final String variant, final String version)
	{
		for(Project project : retrieveProjectsColl())
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
		for(Project project : retrieveProjectsColl())
			if(project.isV1xProject() && project.getID() == schemaID && project.getSchemaVersion() == schemaVersion)
				return project;
		return null;
	}
	
	/**
	 * Retrieves specific Project
	 * 
	 * @return null if project was not found
	 */
	@Override
	public Project retrieveProject(final long projectHash)
	{		
		// Get project from cache if it is there ...
		if(projectCache != null && projectCache.containsKey(projectHash))
			return projectCache.get(projectHash);
		
		// ... parse the project if not ...
		String prefKey = getPrefKey(projectHash);
		String folderPath = preferences.getString(prefKey, null);
		if(folderPath != null)
		{
			Project p = parseProject(folderPath);
			if(p != null)
			{
				cacheProject(p); // cache the project (the cache will be initialised if needed)
				return p;
			}
			else
				removeProjectPathPrefKey(projectHash); // we were unable to parse a project at the path, so remove it from the preferences 
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
		// Remove from cache:
		if(projectCache != null)
			projectCache.remove(project.getHash());
		// Remove from prefs:
		removeProjectPathPrefKey(project);
	}
	
	private void initProjectCache()
	{
		if(projectCache == null)
			projectCache = new HashMap<Long, Project>();
	}
	
	private void updateProjectCache()
	{
		// Ensure we have a cache at all:
		initProjectCache();
		// Loop through project path preference keys:
		for(Map.Entry<String, ?> entry : preferences.getAll().entrySet())
			if(entry.getKey().startsWith(PREF_PROJECT_PATH_PREFIX) && entry.getKey().endsWith(PREF_PROJECT_PATH_POSTFIX))
			{
				long projectHash = getProjectHash(entry.getKey());
				if(!projectCache.containsKey(projectCache))
				{	// Parse the project if it is not already in the cache:
					Project p = parseProject(entry.getValue().toString());
					if(p != null)
					{
						if(p.getHash() != projectHash)
						{
							Log.w(TAG, "Hash code of project " + p.toString() + " has changed, probably the " + ProjectLoader.PROJECT_FILE + " file (located in " + entry.getValue().toString() + ") was manually edited!");
							// Remove old pref key:
							removeProjectPathPrefKey(projectHash);
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
		projectCache.put(project.getHash(), project);
	}
	
	private void removeProjectPathPrefKey(Project project)
	{
		preferences.edit().remove(getPrefKey(project)).commit();
	}
	
	private void removeProjectPathPrefKey(long projectHash)
	{
		preferences.edit().remove(getPrefKey(projectHash)).commit();
	}
	
	private void storeProjectPathPrefKey(Project project)
	{
		preferences.edit().putString(getPrefKey(project), project.getProjectFolderPath()).commit();
	}
	
	private String getPrefKey(Project project)
	{
		return getPrefKey(project.getHash());
	}
	
	private String getPrefKey(long projectHash)
	{
		return PREF_PROJECT_PATH_PREFIX + projectHash + PREF_PROJECT_PATH_POSTFIX;
	}
	
	private long getProjectHash(String projectPathPrefKey)
	{
		return Long.parseLong(projectPathPrefKey.substring(PREF_PROJECT_PATH_PREFIX.length(), projectPathPrefKey.length() - PREF_PROJECT_PATH_POSTFIX.length()));
	}
	
	private Project parseProject(String folderPath)
	{
		File xmlFile = new File(folderPath.toString() + ProjectLoader.PROJECT_FILE);
		// Use the path where the xml file resides as the basePath (img&snd folders are assumed to be in the same place), no subfolders are created:
		ProjectParser parser = new ProjectParser(xmlFile.getParentFile().getAbsolutePath(), false);
		try
		{
			return parser.parseProject(xmlFile);
		}
		catch(Exception e)
		{
			e.printStackTrace();
		}
		return null;
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
