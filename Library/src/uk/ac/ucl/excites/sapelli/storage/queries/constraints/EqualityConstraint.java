/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries.constraints;

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;

/**
 * Constraint that compares the values in a column using Object#equals()
 * 
 * @author mstevens
 */
public class EqualityConstraint extends Constraint
{
	
	private final ColumnPointer columnPointer;
	private final Object value;
	
	public EqualityConstraint(Column<?> column, Object value)
	{
		this(new ColumnPointer(column), value);
	}
	
	public EqualityConstraint(ColumnPointer columnPointer, Object value)
	{
		this.columnPointer = columnPointer;
		this.value = value;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.Filter#_select(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	protected boolean isValid(Record record)
	{
		return Record.EqualValues(columnPointer.retrieveValue(record), value);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#accept(uk.ac.ucl.excites.sapelli.storage.queries.constraints.ConstraintVisitor)
	 */
	@Override
	public void accept(ConstraintVisitor visitor)
	{
		visitor.visit(this);
	}
	
	public ColumnPointer getColumnPointer()
	{
		return columnPointer;
	}
	
	public Object getValue()
	{
		return value;
	}

}
