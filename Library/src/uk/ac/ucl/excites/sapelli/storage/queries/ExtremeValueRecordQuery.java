/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries;

import java.util.Collections;
import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.ComparatorColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;

/**
 * @author mstevens
 */
public class ExtremeValueRecordQuery extends SingleRecordQuery
{

	// STATICS-------------------------------------------------------
	static public ExtremeValueRecordQuery Max(ComparatorColumn<?> column)
	{
		return Max(column, null);
	}
	
	static public ExtremeValueRecordQuery Max(ComparatorColumn<?> column, RecordsQuery recordsQuery)
	{
		return Max(new ColumnPointer(column), recordsQuery);
	}
	
	static public ExtremeValueRecordQuery Max(ColumnPointer columnPointer)
	{
		return new ExtremeValueRecordQuery(columnPointer, true, null);
	}
	
	static public ExtremeValueRecordQuery Max(ColumnPointer columnPointer, RecordsQuery recordsQuery)
	{
		return new ExtremeValueRecordQuery(columnPointer, true, recordsQuery);
	}
	
	static public ExtremeValueRecordQuery Min(ComparatorColumn<?> column)
	{
		return Min(column, null);
	}
	
	static public ExtremeValueRecordQuery Min(ComparatorColumn<?> column, RecordsQuery recordsQuery)
	{
		return Min(new ColumnPointer(column));
	}
	
	static public ExtremeValueRecordQuery Min(ColumnPointer columnPointer)
	{
		return new ExtremeValueRecordQuery(columnPointer, false, null);
	}
	
	static public ExtremeValueRecordQuery Min(ColumnPointer columnPointer, RecordsQuery recordsQuery)
	{
		return new ExtremeValueRecordQuery(columnPointer, false, recordsQuery);
	}
	
	// DYNAMICS------------------------------------------------------
	private ColumnPointer columnPointer;
	private boolean max;
	
	private ExtremeValueRecordQuery(ColumnPointer columnPointer, boolean max, RecordsQuery recordQuery)
	{
		super(recordQuery);
		if(!(columnPointer.getColumn() instanceof ComparatorColumn))
			throw new IllegalArgumentException("Min/max values can only be determined for a " + ComparatorColumn.class.getSimpleName() + "!");
		this.columnPointer = columnPointer;
		this.max = max;
	}
	
	/**
	 * @return the columnPointer
	 */
	public ColumnPointer getColumnPointer()
	{
		return columnPointer;
	}

	/**
	 * @return the compareColumn
	 */
	public ComparatorColumn<?> getCompareColumn()
	{
		return (ComparatorColumn<?>) columnPointer.getColumn();
	}
	
	/**
	 * @return the max
	 */
	public boolean isMax()
	{
		return max;
	}

	@Override
	protected Record reduce(List<Record> records)
	{
		return max ? Collections.max(records, columnPointer) : Collections.min(records, columnPointer);
	}

	@Override
	public Record acceptExecutor(Executor executor)
	{
		return executor.execute(this);
	}

}
