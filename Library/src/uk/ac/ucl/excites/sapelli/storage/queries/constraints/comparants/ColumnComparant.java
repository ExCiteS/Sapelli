/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries.constraints.comparants;

import uk.ac.ucl.excites.sapelli.storage.model.ComparableColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSet;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;

/**
 * @author mstevens
 */
public class ColumnComparant extends ComparingComparant<Object, Object>
{

	private final ColumnPointer<ComparableColumn<?>> columnPointer;
	
	public ColumnComparant(ComparableColumn<?> column)
	{
		this(new ColumnPointer<ComparableColumn<?>>(column));
	}
	
	/**
	 * @param columpPointer
	 */
	public ColumnComparant(ColumnPointer<ComparableColumn<?>> columnPointer)
	{
		this.columnPointer = columnPointer;
	}

	@Override
	public Object getValue(Record record)
	{
		ValueSet<?> valueSet = columnPointer.getValueSet(record, false);
		if(valueSet == null)
			return null;
		else
			return columnPointer.getColumn().retrieveValue(valueSet);
	}

	@Override
	public int compare(Record record, Comparant<? extends Object> another)
	{
		return columnPointer.getColumn().retrieveAndCompareToObject(record, another.getValue(record));
	}

}
