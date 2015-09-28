/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries.constraints.comparants;

import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * A literal valued {@link Comparant}
 * 
 * @author mstevens
 *
 * @param <V>
 */
public class LiteralComparant<V> extends Comparant<V>
{
	
	private final V value;

	/**
	 * @param value
	 */
	public LiteralComparant(V value)
	{
		this.value = value;
	}

	@Override
	public V getValue(Record record)
	{
		return value;
	}

}
