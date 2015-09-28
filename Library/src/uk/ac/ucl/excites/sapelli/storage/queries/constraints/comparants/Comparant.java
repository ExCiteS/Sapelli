/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries.constraints.comparants;

import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * @author mstevens
 *
 * @param <V>
 */
public abstract class Comparant<V>
{
	
	public abstract V getValue(Record record);

}
