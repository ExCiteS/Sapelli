/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage;

import uk.ac.ucl.excites.sapelli.storage.model.Record;
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
	
	/**
	 * Returns a newly created {@link Record} for the given {@link Schema}.
	 * <br/><br/>
	 * A standard implementation would be:
	 * <pre>return new Record(schema);</pre>
	 * But implementing classes can also return an instance of a subclass of {@link Record} for instance.
	 * 
	 * @param schema
	 * @return
	 */
	public Record getNewRecord(Schema schema);
	
}
