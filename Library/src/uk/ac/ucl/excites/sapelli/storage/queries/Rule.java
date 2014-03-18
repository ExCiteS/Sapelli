/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.ComparatorColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.RecordColumn;

/**
 * Class representing a rule or constraint on the values of a specific column
 * 
 * @author mstevens
 */
public class Rule
{
	
	public static enum Comparison
	{
		SMALLER, 			/* < */
		SMALLER_OR_EQUAL,	/* <= */
		EQUAL,				/* = */
		NOT_EQUAL,			/* != */
		GREATER_OR_EQUAL,	/* >= */
		GREATER				/* > */
	}
	
	public static Rule FromString(ComparatorColumn<?> column, Comparison comparison, String valueString) throws IllegalArgumentException, NullPointerException, ParseException
	{
		return new Rule(column, comparison, column.parse(valueString));
	}
	
	private List<RecordColumn<?>> recordColumns;
	private ComparatorColumn<?> compareColumn;
	private Comparison comparison;
	private Object value;
	
	/**
	 * compareColumn must be a top-level column.
	 * 
	 * @param compareColumn
	 * @param comparison
	 * @param value
	 */
	public Rule(ComparatorColumn<?> compareColumn, Comparison comparison, Object value)
	{
		this(compareColumn, comparison, value, (RecordColumn[]) null);
	}
	
	/**
	 * If recordColumns are passed the first one must be a top-level column and each
	 * subsequent one must be a part of the schema of the previous one.
	 * The compareColumn must be a part of the schema of the last recordColumn that is passed,
	 * or if no recordColumns are passed it must be a top-level column itself. 
	 * 
	 * @param compareColumn
	 * @param comparison
	 * @param value
	 * @param recordColumns
	 */
	public Rule(ComparatorColumn<?> compareColumn, Comparison comparison, Object value, RecordColumn<?>... recordColumns)
	{
		this.compareColumn = compareColumn;
		this.comparison = comparison;
		this.value = value;
		this.recordColumns = new ArrayList<RecordColumn<?>>();
		if(recordColumns != null)
			for(RecordColumn<?> rCol : recordColumns)
				this.recordColumns.add(rCol);
	}
	
	/**
	 * @return the recordColumns
	 */
	public List<RecordColumn<?>> getRecordColumns()
	{
		return recordColumns;
	}

	/**
	 * @return the compareColumn
	 */
	public ComparatorColumn<?> getCompareColumn()
	{
		return compareColumn;
	}

	/**
	 * @return the comparison
	 */
	public Comparison getComparison()
	{
		return comparison;
	}

	/**
	 * @return the value
	 */
	public Object getValue()
	{
		return value;
	}
	
	public boolean isValid(Record record)
	{
		// Get down to the right sub-record:
		for(RecordColumn<?> rCol : recordColumns)
		{
			if(record != null)
				record = rCol.retrieveValue(record);
			else
				break;
		}
		// Null check:
		if(record == null)
			return false;
		// Compare value:
		int compResult = compareColumn.retrieveAndCompareToObject(record, value);
		switch(comparison)
		{
			case SMALLER:
				if(compResult >= 0)
					return false; // record-value is not smaller than value (it is larger or equal)
				break;
			case SMALLER_OR_EQUAL:
				if(compResult > 0)
					return false; // record-value is not smaller than or to value (it is larger)
				break;
			case EQUAL:
				if(compResult != 0)
					return false; // record-value is not equal to value (it is larger or smaller)
				break;
			case NOT_EQUAL:
				if(compResult == 0)
					return false; // record-value is not different from value (it is equal)
				break;
			case GREATER_OR_EQUAL:
				if(compResult < 0)
					return false; // record-value is not greater than or equal to value (it is smaller)
				break;
			case GREATER:
				if(compResult <= 0)
					return false; // record-value is not greater than value (it is smaller or equal)
				break;
		}
		return true;
	}
	
}