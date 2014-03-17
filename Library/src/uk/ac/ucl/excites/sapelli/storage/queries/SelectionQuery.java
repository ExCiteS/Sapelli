/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries;

import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * @author mstevens
 *
 */
public abstract class SelectionQuery
{
	
	public boolean select(Record record)
	{
		return record != null & isValid(record);
	}
	
	protected abstract boolean isValid(Record record);
	
	protected abstract void accept(QueryBuilder builder);

}
