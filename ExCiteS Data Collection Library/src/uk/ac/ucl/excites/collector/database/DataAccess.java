/**
 * 
 */
package uk.ac.ucl.excites.collector.database;

import java.util.List;

import uk.ac.ucl.excites.collector.project.model.Form;
import uk.ac.ucl.excites.collector.project.model.Project;
import uk.ac.ucl.excites.collector.project.util.DuplicateException;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;
import uk.ac.ucl.excites.transmission.Transmission;
import uk.ac.ucl.excites.transmission.sms.SMSAgent;
import uk.ac.ucl.excites.transmission.sms.SMSTransmission;
import uk.ac.ucl.excites.transmission.sms.SMSTransmissionID;

import com.db4o.ObjectContainer;
import com.db4o.ObjectSet;
import com.db4o.query.Predicate;


/**
 * @author mstevens, julia, Michalis Vitos
 * 
 */
/**
 * @author mstevens
 *
 */
/**
 * @author mstevens
 *
 */
/**
 * @author mstevens
 *
 */
public final class DataAccess
{

	// Statics----------------------------------------------
	static protected final String TAG = "DATA ACCESS";
	static public final int ACTIVATION_DEPTH = 40;
	static public final int UPDATE_DEPTH = 40;

	// Dynamics---------------------------------------------
	private ObjectContainer db;
	
	public DataAccess(ObjectContainer db)
	{
		this.db = db;
	}
	
	public void commit()
	{
		db.commit();
	}

	/**
	 * @param schema
	 */
	public void store(Schema schema)
	{
		storeObject(schema);
	}

	/**
	 * Retrieves all schemata
	 * 
	 * @return
	 */
	public List<Schema> retrieveSchemata()
	{
		List<Schema> result = db.query(Schema.class);
		for(Schema s : result)
			db.activate(s, ACTIVATION_DEPTH);
		return result;
	}
	
	/**
	 * @param id
	 * @param version
	 * @return
	 */
	public Schema retrieveSchema(final int id, final int version)
	{
		ObjectSet<Schema> result = db.query(new Predicate<Schema>()
		{
			private static final long serialVersionUID = 1L;
			
			public boolean match(Schema schema)
			{
				return schema.getID() == id && schema.getVersion() == version;
			}
		});
		if(result.hasNext())
		{
			Schema s = result.next();
			db.activate(s, ACTIVATION_DEPTH);
			return s;
		}
		else
			return null;
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
	 * @param sendingAttemptTimeoutMinutes - amout of time to waiting after the sending attempt until we can considering it failed (because sent=false)
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
		if(retrieveProject(project.getName(), project.getVersion()) != null)
			throw new DuplicateException("There is already a project named \"" + project.getName() + "\", with version " + project.getVersion() + ". Either remove the existing one or increment the version of the new one.");
		// Check for schema duplicates:
		for(Form f : project.getForms())
		{
			Schema existingSchema = retrieveSchema(f.getSchemaID(), f.getSchemaVersion());
			if(existingSchema != null)
			{	// the form uses a schema (ID/version) which is already known
				try
				{	// try to use the existing schema on the form
					f.setSchema(existingSchema);
				}
				catch(IllegalArgumentException iae)
				{	// existing schema was refused, it does not match with the schema defined by the form itself
					throw new DuplicateException("There is already a project/form (or saved records) with schema ID " + f.getSchemaID() + " and version " + f.getSchemaVersion() + " and the schemas are not compatible.");
				}
			}
		}
		storeObject(project);
	}
	
	/**
	 * @param project
	 */
	public void update(Project project)
	{
		storeObject(project);
	}

	/**
	 * Retrieves all projects
	 * 
	 * @return
	 */
	public List<Project> retrieveProjects()
	{
		final List<Project> result = db.queryByExample(Project.class);
		for(Project p : result)
			db.activate(p, ACTIVATION_DEPTH);
		return result;
	}

	/**
	 * Retrieves specific Project
	 * 
	 * @return null if project was not found
	 */
	public Project retrieveProject(final String name, final String version)
	{
		@SuppressWarnings("serial")
		ObjectSet<Project> result = db.query(new Predicate<Project>()
		{
			public boolean match(Project project)
			{
				return project.getName().equalsIgnoreCase(name) && project.getVersion().equalsIgnoreCase(version);
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
	 * Delete specific project
	 * 
	 * @return
	 */
	public void delete(Project project)
	{
		deleteObject(project);
	}
	
	/**
	 * Retrieve all forms that use/define a schema with the provided ID & version
	 * 
	 * @param schemaID
	 * @param schemaVersion
	 * @return
	 */
	public List<Form> retrieveForms(final int schemaID, final int schemaVersion)
	{
		ObjectSet<Form> result = db.query(new Predicate<Form>()
		{
			private static final long serialVersionUID = 1L;
			
			public boolean match(Form form)
			{
				return form.getSchemaID() == schemaID && form.getSchemaVersion() == schemaVersion;
			}
		});
		for(Form f : result)
			db.activate(f, ACTIVATION_DEPTH);
		return result;
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
