/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector;

import uk.ac.ucl.excites.sapelli.collector.db.ProjectStore;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.model.Project;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
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
public class SapelliCollectorClient extends TransmissionClient
{
	
	// STATICS-------------------------------------------------------
	static public final long COLLECTOR_MANAGEMENT_MODEL_ID = TRANSMISSION_MANAGEMENT_MODEL_ID + 1; // = 1
	
	/**
	 * @param project
	 * @return unsigned 56 bit integer
	 */
	static public long GetModelID(Project project)
	{
		return	((((long) project.hashCode()) & 0xffffffffL) << Project.PROJECT_ID_SIZE) +	// Project hash takes up first 32 bits
				project.getID();															// Project id takes up next 24 bits
	}
	
	static public int GetProjectID(long modelID)
	{
		return (int) (modelID % (1 << Project.PROJECT_ID_SIZE));
	}
	
	static public int GetProjectHash(long modelID)
	{
		return (int) (modelID >> Project.PROJECT_ID_SIZE);
	}
	
	// DYNAMICS------------------------------------------------------
	private ProjectStore projectStore;
	
	public SapelliCollectorClient(ProjectStore projectStore)
	{
		this.projectStore = projectStore;
	}
	
	/**
	 * @param modelID
	 * @return the project corresponding to the given modelID
	 */
	public Project getProject(long modelID)
	{
		return projectStore.retrieveProject(GetProjectID(modelID), GetProjectHash(modelID));
	}
	
	/**
	 * @param schema
	 * @return the form that is backed by the given schema
	 * @throws UnknownModelException when no matching Form is found
	 */
	public Form getForm(Schema schema) throws UnknownModelException
	{
		if(schema.isInternal())
			throw new IllegalArgumentException("Internal schema cannot be associated with a Form");
		Project project = getProject(schema.getModelID());
		if(project != null)
			for(Form f : project.getForms())
				if(f.getSchema().equals(schema))
					return f;
		throw new UnknownModelException(schema.getModelID());
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.StorageClient#getClientModel(long)
	 */
	@Override
	public Model getClientModel(long modelID) throws UnknownModelException
	{
		Project project = getProject(modelID);
		if(project != null)
			return project.getModel();
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
	 * @see uk.ac.ucl.excites.sapelli.transmission.TransmissionClient#getEncryptionSettingsFor(Model)
	 */
	@Override
	public EncryptionSettings getEncryptionSettingsFor(Model model)
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

	@Override
	public Payload newPayload(int nonBuiltinType)
	{
		return null; // for now there are no Sapelli Collector-specific transmission payloads
	}

}
