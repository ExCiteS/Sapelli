/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector;

import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;
import uk.ac.ucl.excites.sapelli.transmission.EncryptionSettings;
import uk.ac.ucl.excites.sapelli.transmission.Payload;
import uk.ac.ucl.excites.sapelli.transmission.TransmissionClient;

/**
 * @author mstevens
 *
 */
public class SapelliCollectorClient implements TransmissionClient
{
	
	// STATICS-------------------------------------------------------
	/**
	 * @param project
	 * @return
	 */
	static public long GetModelID(Project project)
	{
		return GetModelID(project.getID(), project.hashCode());
	}
	
	/**
	 * @param projectID unsigned(!) 24 bit integer
	 * @param projectHash signed 32 bit integer
	 * @return unsigned 56 bit integer
	 */
	static public long GetModelID(int projectID, int projectHash)
	{
		return	((((long) projectHash) & 0xffffffffL) << Project.PROJECT_ID_SIZE) +	// Project hash takes up first 32 bits
				projectID;															// Project id takes up next 24 bits
	}
	
	static public int GetProjectID(long modelID)
	{
		return (int) (modelID % (1 << Project.PROJECT_ID_SIZE));
	}
	
	static public int GetProjectHash(long modelID)
	{
		return (int) (modelID >> Project.PROJECT_ID_SIZE);
	}
	
	static public short GetModelSchemaNo(Form form)
	{
		// reserve No for heartbeart schema!
		// TODO don't reserve no's for non-data producing forms
		return form.getPosition();
	}
	
	// DYNAMICS------------------------------------------------------
	private ProjectStore projectStore;
	
	public SapelliCollectorClient(ProjectStore projectStore)
	{
		this.projectStore = projectStore;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.StorageClient#getSchema(long, short)
	 */
	@Override
	public Schema getSchema(long modelID, short modelSchemaNo) throws UnknownModelException
	{
		Project project = projectStore.retrieveProject(GetProjectID(modelID), GetProjectHash(modelID));
		if(project != null)
			return project.getForm(modelSchemaNo).getSchema();
		else
			throw new UnknownModelException(modelID, modelSchemaNo);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.StorageClient#getNumberOfSchemataInModel(long)
	 */
	@Override
	public short getNumberOfSchemataInModel(long modelID) throws UnknownModelException
	{
		Project project = projectStore.retrieveProject(GetProjectID(modelID), GetProjectHash(modelID));
		if(project != null)
			return (short) project.getForms().size();
		else
			throw new UnknownModelException(modelID);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.StorageClient#getSchemaV1(int, int)
	 */
	@Override
	public Schema getSchemaV1(int schemaID, int schemaVersion) throws UnknownModelException
	{
		Project project = projectStore.retrieveV1Project(schemaID, schemaVersion);
		if(project != null)
			return project.getForm(0).getSchema(); // return schema of the first (and assumed only) form
		else
			throw new UnknownModelException(schemaID, schemaVersion);
	}

	@Override
	public void recordInserted(Record record)
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public void recordUpdated(Record record)
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public void recordDeleted(Record record)
	{
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.transmission.TransmissionClient#getEncryptionSettingsFor(long)
	 */
	@Override
	public EncryptionSettings getEncryptionSettingsFor(long modelID)
	{
		/*TODO FIX THIS
		 * This is buggy/hacky! Because schema's can be shared by multiple forms (and no schema ID/version duplicates are allowed)
		 * we cannot safely determine transmission settings based on the schema id/version.
		 */
//		List<Form> forms = dao.retrieveForms(schema.getID(), schema.getVersion());
//		if(!forms.isEmpty())
//		{
//			if(forms.get(0)/*HACK!*/.getProject() != null)
//				return forms.get(0).getProject().getTransmissionSettings();
//			else
//				return null;
//		}
//		else
//		{
//			return null;
//		}
		return null;
	}
	
	/**
	 * @param schema
	 * @return the form that is backed by the given schema
	 * @throws IllegalArgumentException when no matching Form is found
	 */
	public Form getForm(Schema schema) throws IllegalArgumentException
	{
		long modelID = schema.getModelID();
		Project project = projectStore.retrieveProject(GetProjectID(modelID), GetProjectHash(modelID));
		if(project != null)
			return project.getForm(schema.getModelSchemaNo());
		else
			throw new IllegalArgumentException("No matching form found!");
	}

	@Override
	public Payload newPayload(int nonBuiltinType)
	{
		return null; // for now there are no Sapelli Collector-specific transmission payloads
	}

}
