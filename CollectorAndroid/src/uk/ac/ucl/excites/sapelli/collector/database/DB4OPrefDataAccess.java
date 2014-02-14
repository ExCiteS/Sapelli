/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.database;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import uk.ac.ucl.excites.sapelli.collector.database.db4o.DB4ODataAccess;
import uk.ac.ucl.excites.sapelli.collector.project.io.ProjectLoader;
import uk.ac.ucl.excites.sapelli.collector.project.model.Project;
import uk.ac.ucl.excites.sapelli.collector.project.util.DuplicateException;
import uk.ac.ucl.excites.sapelli.collector.project.xml.ProjectParser;
import uk.ac.ucl.excites.sapelli.util.CollectionUtils;
import android.content.Context;
import android.content.SharedPreferences;

import com.db4o.ObjectContainer;

/**
 * Subclass of DB4ODataAccess in which project storage uses Android SharePreferences (+ reparsing XML) to store Projects (instead of DB4O object storage)
 * 
 * @author Michalis Vitos, mstevens
 */
public class DB4OPrefDataAccess extends DB4ODataAccess
{
	
	private static final String PREF_PROJECT_PATH_POSTFIX = "_PATH";
	// Statics----------------------------------------------
	private static final String PREFERENCES = "PROJECT_PREFERENCES";
	private static final String PREF_PROJECT_PREFIX = "PROJECT_";

	// Dynamics---------------------------------------------
	private Context context;
	private SharedPreferences preferences;

	public DB4OPrefDataAccess(ObjectContainer db, Context context)
	{
		super(db);
		this.context = context;
		this.preferences = this.context.getSharedPreferences(PREFERENCES, Context.MODE_PRIVATE);
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
		SharedPreferences.Editor editor = preferences.edit();
		editor.putString(getProjectPathPrefKey(project), project.getProjectFolderPath());
		editor.commit();
	}
	
	/**
	 * Retrieves all projects
	 * 
	 * @return
	 */
	@Override
	public List<Project> retrieveProjects()
	{
		List<Project> projects = new ArrayList<Project>();
		for(Map.Entry<String, ?> entry : preferences.getAll().entrySet())
			if(entry.getKey().startsWith(PREF_PROJECT_PREFIX) && entry.getKey().endsWith(PREF_PROJECT_PATH_POSTFIX))
				CollectionUtils.addIgnoreNull(projects, parseProject(entry.getValue().toString()));
		return projects;
	}
	
	/**
	 * Retrieves specific Project
	 * 
	 * @return null if project was not found
	 */
	@Override
	public Project retrieveProject(final String name, final String variant, final String version)
	{
		List<Project> projects = retrieveProjects();
		for(Project project : projects)
		{
			if( project.getName().equalsIgnoreCase(name)
				&& (variant != null ? variant.equals(project.getVariant()) : true)
			    && project.getVersion().equalsIgnoreCase(version))
				return project;
		}
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
		String folderPath = preferences.getString(getProjectPathPrefKey(projectHash), null);
		if(folderPath != null)
			return parseProject(folderPath);
		else
			return null;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.database.IDataAccess#retrieveV1Project(int, int)
	 */
	@Override
	public Project retrieveV1Project(final int schemaID, final int schemaVersion)
	{
		List<Project> projects = retrieveProjects();
		for(Project project : projects)
			if(project.isV1xProject() && project.getID() == schemaID && project.getSchemaVersion() == schemaVersion)
				return project;
		return null;
	}

	/**
	 * Delete specific project
	 * 
	 * @return
	 */
	@Override
	public void delete(Project project)
	{
		preferences.edit().remove(getProjectPathPrefKey(project)).commit();
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
	
	private String getProjectPathPrefKey(Project project)
	{
		return getProjectPathPrefKey(project.getHash());
	}
	
	private String getProjectPathPrefKey(long projectHash)
	{
		return PREF_PROJECT_PREFIX + projectHash + PREF_PROJECT_PATH_POSTFIX;
	}

}
