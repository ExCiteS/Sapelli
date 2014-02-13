/**
 * 
 */
package uk.ac.ucl.excites.sapelli.transmission;

import java.util.Set;

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;

/**
 * @author mstevens
 *
 */
public interface ModelProvider
{

	public Schema getSchema(int id, int version);
	
	public Settings getSettingsFor(Schema schema);
	
	public Set<Column<?>> getFactoredOutColumnsFor(Schema schema);
	
}
