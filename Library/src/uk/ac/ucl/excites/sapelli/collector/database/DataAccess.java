package uk.ac.ucl.excites.sapelli.collector.database;

import java.util.List;

import uk.ac.ucl.excites.sapelli.collector.project.model.Project;
import uk.ac.ucl.excites.sapelli.collector.project.util.DuplicateException;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.transmission.Transmission;
import uk.ac.ucl.excites.sapelli.transmission.sms.SMSAgent;
import uk.ac.ucl.excites.sapelli.transmission.sms.SMSTransmission;

/**
 * @author mstevens
 *
 */
public interface DataAccess
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
	 * @param record - the record to store
	 */
	public abstract void store(Record record);

	/**
	 * @param record - the record to delete
	 */
	public abstract void delete(Record record);

	/**
	 * Retrieve all Records
	 * 
	 * @return
	 */
	public abstract List<Record> retrieveRecords();

	/**
	 * Deletes *ALL* records.
	 * USE WITH CARE!
	 */
	public abstract void deleteAllRecords();

	/**
	 * Retrieve Records by their Schema
	 * 
	 * @param schema
	 * @return
	 */
	public abstract List<Record> retrieveRecords(Schema schema);

	/**
	 * Retrieve unsent records of a given Schema
	 * 
	 * @param schema
	 * @param sendingAttemptTimeoutMinutes - amount of time to waiting after the sending attempt until we can considering it failed (because sent=false)
	 * @return
	 */
	public abstract List<Record> retrieveUnsentRecords(Schema schema, int sendingAttemptTimeoutMinutes);

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
	public abstract Project retrieveProject(String name, String version);

	/**
	 * Retrieves specific Project
	 * 
	 * @return null if project was not found
	 */
	public abstract Project retrieveProject(String name, String variant, String version);

	/**
	 * Retrieves specific Project
	 * 
	 * @return null if project was not found
	 */
	public abstract Project retrieveProject(long projectHash);

	/**
	 * Delete specific project
	 * 
	 * @return
	 */
	public abstract void delete(Project project);

	/**
	 * @param transmission
	 */
	public abstract void store(Transmission transmission);

	/**
	 * Retrieves all transmissions
	 * 
	 * @return
	 */
	public abstract List<Transmission> retrieveTransmissions();

	/**
	 * Retrieves all unsent transmissions
	 * 
	 * @return
	 */
	public abstract List<Transmission> retrieveUnsentTransmissions();

	/**
	 * Retrieve incomplete SMSTransmission by sender & id
	 * 
	 * @param sender
	 * @param id
	 * @return
	 */
	public abstract SMSTransmission retrieveIncompleteSMSTransmission(SMSAgent sender, int id);

}