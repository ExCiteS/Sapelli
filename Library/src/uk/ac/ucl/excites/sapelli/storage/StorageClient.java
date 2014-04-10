/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage;

import uk.ac.ucl.excites.sapelli.storage.model.Schema;

/**
 * @author mstevens
 *
 */
public interface StorageClient
{

	/**
	 * @param id
	 * @return	a matching {@link Schema} instance, or {@code null} if not found
	 */
	public Schema getSchema(long id);
	
	/**
	 * @param schemaID
	 * @param schemaVersion
	 * @return	a matching {@link Schema} instance, or {@code null} if not found
	 */
	public Schema getSchemaV1(int schemaID, int schemaVersion);
		
}
