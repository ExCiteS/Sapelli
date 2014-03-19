/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * @author mstevens
 *
 */
public abstract class SelectionQuery
{
	
	public List<Record> filter(List<Record> records)
	{
		List<Record> result = new ArrayList<Record>();
		for(Record r : records)
			if(select(r))
				result.add(r);
		return result;
	}

	public boolean select(Record record)
	{
		return record != null & isValid(record);
	}
	
	protected abstract boolean isValid(Record record);
	
	protected abstract void accept(QueryBuilder builder);

}
