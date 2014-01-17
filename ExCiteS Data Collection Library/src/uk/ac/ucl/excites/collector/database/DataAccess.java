/**
 * 
 */
package uk.ac.ucl.excites.collector.database;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.util.DuplicateException;
import uk.ac.ucl.excites.collector.project.xml.ProjectParser;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.transmission.Transmission;
import uk.ac.ucl.excites.transmission.sms.SMSAgent;
import uk.ac.ucl.excites.transmission.sms.SMSTransmission;
import uk.ac.ucl.excites.transmission.sms.SMSTransmissionID;
import android.content.Context;
import android.content.SharedPreferences;

import com.db4o.ObjectContainer;
import com.db4o.ObjectSet;
import com.db4o.query.Predicate;

/**
 * @author mstevens, julia, Michalis Vitos
 * 
 */
public final class DataAccess
{

	// Statics----------------------------------------------
	static protected final String TAG = "DATA ACCESS";
	static public final int ACTIVATION_DEPTH = 40;
	static public final int UPDATE_DEPTH = 40;
	static private final String PROJECT_FILE = "PROJECT.xml";

	// Preferences
	private static final String PREFERENCES = "PROJECT_PREFERENCES";
	private static final String PREF_PROJECT = "PROJECT_";

	// Dynamics---------------------------------------------
	private ObjectContainer db;
	private SharedPreferences preferences;
	private Context context;
	
	public DataAccess(ObjectContainer db, Context context)
	{
		this.context = context;
		this.db = db;
		this.preferences = this.context.getSharedPreferences(PREFERENCES, Context.MODE_PRIVATE);
	}
	
	public void commit()
	{
		db.commit();
	}
	
	/**
	 * For backwards compatibility only
	 * 
	 * @param id
	 * @param version
	 * @return
	 */
	public Project retrieveV1Project(final int schemaID, final int schemaVersion)
	{
		@SuppressWarnings("serial")
		ObjectSet<Project> result = db.query(new Predicate<Project>()
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
			db.activate(p, ACTIVATION_DEPTH);
			return p;
		}
	}
	
	/**
	 * @param record - the record to store
	 */
	public void store(Record record)
	{
		storeObject(record);
	}
	
	/**
	 * @param record - the record to delete
	 */
	public void delete(Record record)
	{
		deleteObject(record);
	}
	
	/**
	 * Retrieve all Records
	 * 
	 * @return
	 */
	public List<Record> retrieveRecords()
	{
		List<Record> result = db.query(Record.class);
		for(Record r : result)
			db.activate(r, ACTIVATION_DEPTH);
		return result;
	}
	
	/**
	 * Deletes *ALL* records.
	 * USE WITH CARE!
	 */
	public void deleteAllRecords()
	{
		List<Record> result = db.query(Record.class);
		for(Record r : result)
			db.delete(r);
		db.commit();
	}
	
	/**
	 * Retrieve Records by their Schema
	 * 
	 * @param schema
	 * @return
	 */
	public List<Record> retrieveRecords(final Schema schema)
	{
		ObjectSet<Record> result = db.query(new Predicate<Record>()
		{
			private static final long serialVersionUID = 1L;
			
			public boolean match(Record record)
			{
				return record.getSchema() == schema;
			}
		});
		for(Record r : result)
			db.activate(r, ACTIVATION_DEPTH);
		return result;
	}
	
	/**
	 * Retrieve unsent records of a given Schema
	 * 
	 * @param schema
	 * @param sendingAttemptTimeoutMinutes - amount of time to waiting after the sending attempt until we can considering it failed (because sent=false)
	 * @return
	 */
	public List<Record> retrieveUnsentRecords(final Schema schema, final int sendingAttemptTimeoutMinutes)
	{
		ObjectSet<Record> result = db.query(new Predicate<Record>()
		{
			private static final long serialVersionUID = 1L;
			
			public boolean match(Record r)
			{
				return	r.getSchema() == schema &&
						!r.isSent() &&
						(r.getSendingAttemptedAt() == null || r.getSendingAttemptedAt().plusMinutes(sendingAttemptTimeoutMinutes).isBeforeNow());
			}
		});
		for(Record r : result)
			db.activate(r, ACTIVATION_DEPTH);
		return result;
	}
	
	/**
	 * @param project
	 */
	public void store(Project project) throws DuplicateException
	{
		// Check for project duplicates:
		if(retrieveProject(project.getName(), project.getVariant(), project.getVersion()) != null)
			throw new DuplicateException("There is already a project named \"" + project.getName() + "\", with version " + project.getVersion() + ". Either remove the existing one or increment the version of the new one.");

		SharedPreferences.Editor editor = preferences.edit();
		editor.putString(PREF_PROJECT + project.getHash() + "_PATH", project.getProjectFolderPath());
		editor.commit();
	}
	
	/**
	 * Retrieves all projects
	 * 
	 * @return
	 */
	public List<Project> retrieveProjects()
	{
		List<Project> projects = new ArrayList<Project>();
		Map<String, ?> keys = preferences.getAll();

		for(Map.Entry<String, ?> entry : keys.entrySet())
			projects.add(parseProject(entry.getValue().toString()));

		return projects;
	}
	
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
	public Project retrieveProject(final String name, final String variant, final String version)
	{
		List<Project> projects = retrieveProjects();
		
		for(Project project : projects)
		{
			if( project.getName().equalsIgnoreCase(name) && (variant != null ? variant.equals(project.getVariant()) : true)
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
	public Project retrieveProject(final long projectHash)
	{
		List<Project> projects = retrieveProjects();

		for(Project project : projects)
		{
			if(project.getHash() == projectHash)
				return project;
		}

		return null;
	}

	private Project parseProject(String file)
	{
		File xmlFile = new File(file.toString() + PROJECT_FILE);
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

	/**
	 * Delete specific project
	 * 
	 * @return
	 */
	public void delete(Project project)
	{
		List<Project> projects = retrieveProjects();

		for(Project p : projects)
		{
			if(equals(project, p))
				preferences.edit().remove(PREF_PROJECT + project.getHash() + "_PATH").commit();
		}
	}
	
	private boolean equals(Project first, Project second)
	{
		// if the two objects are equal in reference, they are equal
		if(first == second)
			return true;
		else if(first.getName().equals(second.getName())
				&& (((first.getVariant() != null) && (second.getVariant() != null)) ? first.getVariant().equals(second.getVariant()) : true)
				&& first.getVersion().equals(second.getVersion()))
			return true;
		else
			return false;
	}

	/**
	 * @param transmission
	 */
	public void store(Transmission transmission)
	{
		storeObject(transmission);
	}
	
	/**
	 * Retrieves all transmissions
	 * 
	 * @return
	 */
	public List<Transmission> retrieveTransmissions()
	{
		final List<Transmission> result = db.queryByExample(Transmission.class);
		for(Transmission t : result)
			db.activate(t, ACTIVATION_DEPTH);
		return result;
	}

	/**
	 * Retrieves all unsent transmissions
	 * 
	 * @return
	 */
	public List<Transmission> retrieveUnsentTransmissions()
	{
		@SuppressWarnings("serial")
		ObjectSet<Transmission> result = db.query(new Predicate<Transmission>()
		{
			public boolean match(Transmission t)
			{
				return !t.isSent();
			}
		});
		for(Transmission t : result)
			db.activate(t, ACTIVATION_DEPTH);
		return result;
	}
	
	/**
	 * Retrieve incomplete SMSTransmission by sender & id
	 * 
	 * @param sender
	 * @param id
	 * @return
	 */
	public SMSTransmission retrieveIncompleteSMSTransmission(final SMSAgent sender, final int id)
	{
		@SuppressWarnings("serial")
		ObjectSet<SMSTransmission> result = db.query(new Predicate<SMSTransmission>()
		{
			public boolean match(SMSTransmission t)
			{
				return !t.isComplete() && t.getSender().equals(sender) && t.getID() == id ;
			}
		});
		if(result.hasNext())
		{
			SMSTransmission t = result.next();
			db.activate(t, ACTIVATION_DEPTH);
			return t;
		}
		else
			return null;
	}
	
	/**
	 * @param id
	 */
	public void store(SMSTransmissionID id)
	{
		storeObject(id);
	}
	
	public SMSTransmissionID retrieveTransmissionID()
	{
		List<SMSTransmissionID> result = db.query(SMSTransmissionID.class);
		if(result.isEmpty())
			return null;
		return result.get(0);
	}

	public void storeObject(Object obj)
	{
		storeObject(obj, true);
	}
	
	public void storeObject(Object obj, boolean commit)
	{
		db.store(obj);
		if(commit)
			db.commit();
	}
	
	public void deleteObject(Object obj)
	{
		deleteObject(obj, true);
	}
	
	public void deleteObject(Object obj, boolean commit)
	{
		db.delete(obj);
		if(commit)
			db.commit();
		db.commit();
	}
	
}
