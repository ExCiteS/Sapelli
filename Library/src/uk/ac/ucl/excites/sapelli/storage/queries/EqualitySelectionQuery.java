/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * @author mstevens
 *
 */
public class EqualitySelectionQuery extends SelectionQuery
{
	
	private final Map<Column<?>, Object> columnToValue;
	
	public EqualitySelectionQuery()
	{
		this.columnToValue = new HashMap<Column<?>, Object>();
	}
	
	public void addColumnValue(Column<?> column, Object value)
	{
		columnToValue.put(column, value);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.RecordSelectionQuery#isValid(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	public boolean isValid(Record record)
	{
		for(Column<?> c : columnToValue.keySet())
			if(!Record.EqualValues(columnToValue.get(c), c.retrieveValue(record)))
				return false;
		return true;
	}
	
	@Override
	protected void accept(QueryBuilder builder)
	{
		builder.visit(this);
	}
	
	public Set<Column<?>> getColumns()
	{
		return columnToValue.keySet();
	}
	
	public Object getValueFor(Column<?> column)
	{
		return columnToValue.get(column);
	}

}
