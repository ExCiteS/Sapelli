/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries.constraints;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.ComparatorColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.RecordColumn;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;

/**
 * Rule-based Constraint class
 * 
 * @author mstevens
 */
public class RuleConstraint extends Constraint
{
	
	// STATICS-------------------------------------------------------
	public static enum Comparison
	{
		SMALLER, 			/* < */
		SMALLER_OR_EQUAL,	/* <= */
		EQUAL,				/* = */
		NOT_EQUAL,			/* != */
		GREATER_OR_EQUAL,	/* >= */
		GREATER				/* > */
	}
	
	/**
	 * @param compareColumn
	 * @param comparison
	 * @param valueString
	 * @return
	 * @throws IllegalArgumentException
	 * @throws NullPointerException
	 * @throws ParseException
	 */
	public static RuleConstraint FromString(ComparatorColumn<?> compareColumn, Comparison comparison, String valueString) throws IllegalArgumentException, NullPointerException, ParseException
	{
		return FromString(new ColumnPointer(compareColumn), comparison, valueString);
	}
	
	/**
	 * @param columnPointer
	 * @param comparison
	 * @param valueString
	 * @return
	 * @throws IllegalArgumentException
	 * @throws NullPointerException
	 * @throws ParseException
	 */
	public static RuleConstraint FromString(ColumnPointer columnPointer, Comparison comparison, String valueString) throws IllegalArgumentException, NullPointerException, ParseException
	{
		return new RuleConstraint(columnPointer, comparison, columnPointer.getColumn().parse(valueString));
	}
	
	// DYNAMICS------------------------------------------------------
	private List<RecordColumn<?>> recordColumns;
	private ColumnPointer columnPointer;
	private Comparison comparison;
	private Object value;
	
	/**
	 * compareColumn must be a top-level column.
	 * 
	 * @param compareColumn
	 * @param comparison
	 * @param value
	 */
	public RuleConstraint(ComparatorColumn<?> compareColumn, Comparison comparison, Object value)
	{
		this(new ColumnPointer(compareColumn), comparison, value);
	}
	
	/**
	 * @param columnPointer
	 * @param comparison
	 * @param value
	 * @param recordColumns
	 */
	public RuleConstraint(ColumnPointer columnPointer, Comparison comparison, Object value)
	{
		if(!(columnPointer.getColumn() instanceof ComparatorColumn))
			throw new IllegalArgumentException("Rules can only be applied to " + ComparatorColumn.class.getSimpleName() + "s!");
		this.columnPointer = columnPointer;
		this.comparison = comparison;
		this.value = value;
		this.recordColumns = new ArrayList<RecordColumn<?>>();
		if(recordColumns != null)
			for(RecordColumn<?> rCol : recordColumns)
				this.recordColumns.add(rCol);
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
		// Get (sub)record:
		record = columnPointer.getRecord(record, false);
		// Null check:
		if(record == null)
			return false;
		// Compare value:
		int compResult = getCompareColumn().retrieveAndCompareToObject(record, value);
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

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#accept(uk.ac.ucl.excites.sapelli.storage.queries.constraints.ConstraintVisitor)
	 */
	@Override
	public void accept(ConstraintVisitor visitor)
	{
		visitor.visit(this);
	}
	
}