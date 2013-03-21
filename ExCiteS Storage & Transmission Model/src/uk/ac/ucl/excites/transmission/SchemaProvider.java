/**
 * 
 */
package uk.ac.ucl.excites.transmission;

import java.util.Set;

import uk.ac.ucl.excites.storage.model.Column;
import uk.ac.ucl.excites.storage.model.Schema;

/**
 * @author mstevens
 *
 */
public interface SchemaProvider
{

	public Schema getSchema(int id, int version);
	
	public Set<Column<?>> getFactoredOutColumnsFor(Schema schema);
	
}
