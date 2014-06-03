/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage;

import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.util.UnknownModelException;

/**
 * @author mstevens
 *
 */
public interface StorageClient
{

	/**
	 * @param modelID (unsigned 56 bit integer)
	 * @param modelSchemaNo (unsigned 4 bit integer)
	 * @return a matching {@link Schema} instance
	 * @throws {@link UnknownModelException} when no matching schema is found
	 */
	public Schema getSchema(long modelID, short modelSchemaNo) throws UnknownModelException;
	
	/**
	 * Returns the number of schemata that exist in the model with the given {@code modelID}
	 * 
	 * @param modelID
	 * @returns the number of schemata
	 * @throws {@link UnknownModelException} when no matching model is found
	 */
	public short getNumberOfSchemataInModel(long modelID) throws UnknownModelException;
	
	/**
	 * @param schemaID
	 * @param schemaVersion
	 * @return a matching {@link Schema} instance
	 * @throws {@link UnknownModelException} when no matching schema is found
	 */
	public Schema getSchemaV1(int schemaID, int schemaVersion) throws UnknownModelException;
	
	public abstract void recordInserted(Record record);
	
	public abstract void recordUpdated(Record record);
	
	public abstract void recordDeleted(Record record);
	
}
