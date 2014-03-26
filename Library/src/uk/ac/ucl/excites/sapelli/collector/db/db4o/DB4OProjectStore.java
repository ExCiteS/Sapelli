/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.db.db4o;

import java.io.File;
import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.util.DuplicateException;
import uk.ac.ucl.excites.sapelli.shared.db.db4o.DB4OConnector;
import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;

import com.db4o.ObjectContainer;
import com.db4o.ObjectSet;
import com.db4o.query.Predicate;

/**
 * @author mstevens, julia, Michalis Vitos
 * 
 */
public class DB4OProjectStore extends ProjectStore
{

	// Statics----------------------------------------------
	static public final String DATABASE_NAME_SUFFIX = "_Projects";
	static public final String BACKUP_SUFFIX = "_Backup";
	static public final int ACTIVATION_DEPTH = 40;
	static public final int UPDATE_DEPTH = 40;

	// Dynamics---------------------------------------------
	private ObjectContainer db4o;
	private String filename;
	
	public DB4OProjectStore(File folder, String baseFilename) throws Exception
	{
		this.filename = baseFilename + DATABASE_NAME_SUFFIX;
		this.db4o = DB4OConnector.open(DB4OConnector.getFile(folder, filename), Project.class);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#retrieveV1Project(int, int)
	 */
	@Override
	public Project retrieveV1Project(final int schemaID, final int schemaVersion)
	{
		@SuppressWarnings("serial")
		ObjectSet<Project> result = db4o.query(new Predicate<Project>()
		{
			public boolean match(Project project)
			{
				return 	project.isV1xProject() &&
						project.getID() == schemaID &&
						project.getSchemaVersion() == schemaVersion;
			}
		});
		if(result.isEmpty())
			return null;
		else
		{
			Project p = result.get(0);
			db4o.activate(p, ACTIVATION_DEPTH);
			return p;
		}
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#store(uk.ac.ucl.excites.sapelli.collector.model.Project)
	 */
	@Override
	public void store(Project project) throws DuplicateException
	{
		// Check for project duplicates:
		if(retrieveProject(project.getName(), project.getVariant(), project.getVersion()) != null)
			throw new DuplicateException("There is already a project named \"" + project.getName() + "\", with version " + project.getVersion() + ". Either remove the existing one or increment the version of the new one.");
		db4o.store(project);
		db4o.commit();
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#retrieveProjects()
	 */
	@Override
	public List<Project> retrieveProjects()
	{
		final List<Project> result = db4o.queryByExample(Project.class);
		for(Project p : result)
			db4o.activate(p, ACTIVATION_DEPTH);
		return result;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#retrieveProject(java.lang.String, java.lang.String, java.lang.String)
	 */
	@Override
	public Project retrieveProject(final String name, final String variant, final String version)
	{
		@SuppressWarnings("serial")
		ObjectSet<Project> result = db4o.query(new Predicate<Project>()
		{
			public boolean match(Project project)
			{
				return 	project.getName().equalsIgnoreCase(name) &&
						(variant != null ? variant.equals(project.getVariant()) : true) &&
						project.getVersion().equalsIgnoreCase(version);
			}
		});
		if(result.isEmpty())
			return null;
		else
		{
			Project p = result.get(0);
			db4o.activate(p, ACTIVATION_DEPTH);
			return p;
		}
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#retrieveProject(long)
	 */
	@Override
	public Project retrieveProject(final long projectHash)
	{
		@SuppressWarnings("serial")
		ObjectSet<Project> result = db4o.query(new Predicate<Project>()
		{
			public boolean match(Project project)
			{
				return project.getHash() == projectHash;
			}
		});
		if(result.isEmpty())
			return null;
		else
		{
			Project p = result.get(0);
			db4o.activate(p, ACTIVATION_DEPTH);
			return p;
		}
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.db.ProjectStore#delete(uk.ac.ucl.excites.sapelli.collector.model.Project)
	 */
	@Override
	public void delete(Project project)
	{
		db4o.delete(project);
		db4o.commit();
	}

	@Override
	public void finalise()
	{
		db4o.close();
	}

	@Override
	public void backup(File destinationFolder) throws Exception
	{
		db4o.commit();
		db4o.ext().backup(DB4OConnector.getFile(destinationFolder, filename + BACKUP_SUFFIX + "_" + TimeUtils.getTimestampForFileName()).getAbsolutePath());
	}
	
}
