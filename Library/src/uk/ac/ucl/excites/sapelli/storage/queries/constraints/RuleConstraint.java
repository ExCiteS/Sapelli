/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries.constraints;

import java.text.ParseException;

import uk.ac.ucl.excites.sapelli.storage.model.ComparatorColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
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
	
	public static final String[] COMPARISON_STRINGS = new String[]
	{	
		"smaller",
		"smallerEqual",
		"equal",
		"different", "notEqual",
		"greaterEqual",
		"greater"
	};
	
	public static Comparison parseComparisonString(String comparisonString) throws ParseException
	{
		if(COMPARISON_STRINGS[0].equalsIgnoreCase(comparisonString))
			return Comparison.SMALLER;
		if(COMPARISON_STRINGS[1].equalsIgnoreCase(comparisonString))
			return Comparison.SMALLER_OR_EQUAL;
		if(COMPARISON_STRINGS[2].equalsIgnoreCase(comparisonString))
			return Comparison.EQUAL;
		if(COMPARISON_STRINGS[3].equalsIgnoreCase(comparisonString) || COMPARISON_STRINGS[4].equalsIgnoreCase(comparisonString))
			return Comparison.NOT_EQUAL;
		if(COMPARISON_STRINGS[5].equalsIgnoreCase(comparisonString))
			return Comparison.GREATER_OR_EQUAL;
		if(COMPARISON_STRINGS[6].equalsIgnoreCase(comparisonString))
			return Comparison.GREATER;
		throw new ParseException("Unrecognised comparison", 0);
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
	 */
	public RuleConstraint(ColumnPointer columnPointer, Comparison comparison, Object value)
	{
		if(!(columnPointer.getColumn() instanceof ComparatorColumn))
			throw new IllegalArgumentException("Rules can only be applied to " + ComparatorColumn.class.getSimpleName() + "s!");
		this.columnPointer = columnPointer;
		this.comparison = comparison;
		this.value = value;
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
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#_isValid(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	public boolean _isValid(Record record)
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
				return compResult < 0;
			case SMALLER_OR_EQUAL:
				return compResult <= 0;
			case EQUAL:
				return compResult == 0;
			case NOT_EQUAL:
				return compResult != 0;
			case GREATER_OR_EQUAL:
				return compResult >= 0;
			case GREATER:
				return compResult > 0;
			default:
				throw new IllegalStateException("Unknown comparison: " + comparison.name());
		}
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#accept(uk.ac.ucl.excites.sapelli.storage.queries.constraints.ConstraintVisitor)
	 */
	@Override
	public void accept(ConstraintVisitor visitor)
	{
		visitor.visit(this);
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof RuleConstraint)
		{
			RuleConstraint that = (RuleConstraint) obj;
			return	this.columnPointer.equals(that.columnPointer) &&
					this.comparison == that.comparison &&
					this.value.equals(that.value);
		}
		return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + columnPointer.hashCode();
		hash = 31 * hash + comparison.ordinal();
		hash = 31 * hash + value.hashCode();
		return hash;
	}
	
}