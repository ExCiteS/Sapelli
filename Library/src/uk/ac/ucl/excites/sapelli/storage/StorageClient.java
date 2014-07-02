/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;

/**
 * @author mstevens
 *
 */
public abstract class StorageClient
{
	
	// DYNAMICS------------------------------------------------------
	
	public Model getModel(long modelID) throws UnknownModelException
	{
		// First check reserved models:
		for(Model model : getReserveredModels())
			if(model.getID() == modelID)
				return model;
		// Get client model:
		return getClientModel(modelID);
	}
	
	/**
	 * Subclasses can override this but *must* return at least the same models returned by the super implementation.
	 * 
	 * @return
	 */
	public List<Model> getReserveredModels()
	{
		return new ArrayList<Model>();
	}
		
	public abstract Model getClientModel(long modelID) throws UnknownModelException;
	
	/**
	 * @param schemaID
	 * @param schemaVersion
	 * @return a matching {@link Schema} instance
	 * @throws {@link UnknownModelException} when no matching schema is found
	 */
	public abstract Schema getSchemaV1(int schemaID, int schemaVersion) throws UnknownModelException;
	
	public abstract void recordInserted(Record record);
	
	public abstract void recordUpdated(Record record);
	
	public abstract void recordDeleted(Record record);
	
}
