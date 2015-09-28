/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries.constraints.comparants;

import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * A {@link Comparant} which is able to compare its own value with
 * that of another {@link Comparant}, possibly of another generic type.
 * 
 * @author mstevens
 *
 * @param <V> the generic type of the Comparant
 * @param <W> the generic type of the Comparants this one can be compared with 
 */
public abstract class ComparingComparant<V, W> extends Comparant<V>
{
	
	public abstract int compare(Record record, Comparant<? extends W> another);

}
