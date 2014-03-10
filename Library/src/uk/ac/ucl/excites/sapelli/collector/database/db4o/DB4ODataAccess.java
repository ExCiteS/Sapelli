/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.database.db4o;

import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.database.DataAccess;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.collector.util.DuplicateException;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.transmission.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.sms.SMSAgent;
import uk.ac.ucl.excites.sapelli.transmission.sms.SMSTransmission;

import com.db4o.ObjectContainer;
import com.db4o.ObjectSet;
import com.db4o.query.Predicate;

/**
 * @author mstevens, julia, Michalis Vitos
 * 
 */
public class DB4ODataAccess implements DataAccess
{

	// Statics----------------------------------------------
	static protected final String TAG = "DB4ODataAccess";
	static public final int ACTIVATION_DEPTH = 40;
	static public final int UPDATE_DEPTH = 40;

	// Dynamics---------------------------------------------
	private ObjectContainer db;
	
	public DB4ODataAccess(ObjectContainer db)
	{
		this.db = db;
	}
	
	public void commit()
	{
		db.commit();
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.database.IDataAccess#retrieveV1Project(int, int)
	 */
	@Override
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
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.database.IDataAccess#store(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	public void store(Record record)
	{
		storeObject(record);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.database.IDataAccess#delete(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	public void delete(Record record)
	{
		deleteObject(record);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.database.IDataAccess#retrieveRecords()
	 */
	@Override
	public List<Record> retrieveRecords()
	{
		List<Record> result = db.query(Record.class);
		for(Record r : result)
			db.activate(r, ACTIVATION_DEPTH);
		return result;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.database.IDataAccess#deleteAllRecords()
	 */
	@Override
	public void deleteAllRecords()
	{
		List<Record> result = db.query(Record.class);
		for(Record r : result)
			db.delete(r);
		db.commit();
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.database.IDataAccess#retrieveRecords(uk.ac.ucl.excites.sapelli.storage.model.Schema)
	 */
	@Override
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
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.database.IDataAccess#retrieveUnsentRecords(uk.ac.ucl.excites.sapelli.storage.model.Schema, int)
	 */
	@Override
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
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.database.IDataAccess#store(uk.ac.ucl.excites.sapelli.collector.project.model.Project)
	 */
	@Override
	public void store(Project project) throws DuplicateException
	{
		// Check for project duplicates:
		if(retrieveProject(project.getName(), project.getVariant(), project.getVersion()) != null)
			throw new DuplicateException("There is already a project named \"" + project.getName() + "\", with version " + project.getVersion() + ". Either remove the existing one or increment the version of the new one.");
		storeObject(project);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.database.IDataAccess#retrieveProjects()
	 */
	@Override
	public List<Project> retrieveProjects()
	{
		final List<Project> result = db.queryByExample(Project.class);
		for(Project p : result)
			db.activate(p, ACTIVATION_DEPTH);
		return result;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.database.IDataAccess#retrieveProject(java.lang.String, java.lang.String)
	 */
	@Override
	public Project retrieveProject(final String name, final String version)
	{
		return retrieveProject(name, null, version);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.database.IDataAccess#retrieveProject(java.lang.String, java.lang.String, java.lang.String)
	 */
	@Override
	public Project retrieveProject(final String name, final String variant, final String version)
	{
		@SuppressWarnings("serial")
		ObjectSet<Project> result = db.query(new Predicate<Project>()
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
			db.activate(p, ACTIVATION_DEPTH);
			return p;
		}
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.database.IDataAccess#retrieveProject(long)
	 */
	@Override
	public Project retrieveProject(final long projectHash)
	{
		@SuppressWarnings("serial")
		ObjectSet<Project> result = db.query(new Predicate<Project>()
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
			db.activate(p, ACTIVATION_DEPTH);
			return p;
		}
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.database.IDataAccess#delete(uk.ac.ucl.excites.sapelli.collector.project.model.Project)
	 */
	@Override
	public void delete(Project project)
	{
		deleteObject(project);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.database.IDataAccess#store(uk.ac.ucl.excites.sapelli.transmission.Transmission)
	 */
	@Override
	public void store(Transmission transmission)
	{
		storeObject(transmission);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.database.IDataAccess#retrieveTransmissions()
	 */
	@Override
	public List<Transmission> retrieveTransmissions()
	{
		final List<Transmission> result = db.queryByExample(Transmission.class);
		for(Transmission t : result)
			db.activate(t, ACTIVATION_DEPTH);
		return result;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.database.IDataAccess#retrieveUnsentTransmissions()
	 */
	@Override
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
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.collector.database.IDataAccess#retrieveIncompleteSMSTransmission(uk.ac.ucl.excites.sapelli.transmission.sms.SMSAgent, int)
	 */
	@Override
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
