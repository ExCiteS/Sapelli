/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries.constraints;

import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * Negation of a Constraint
 * 
 * @author mstevens
 */
public class NotConstraint extends Constraint
{

	private final Constraint negatedConstraint;
	
	public NotConstraint(Constraint negatedConstraint)
	{
		this.negatedConstraint = negatedConstraint;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#_isValid(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	protected boolean _isValid(Record record)
	{
		return !negatedConstraint._isValid(record);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#accept(uk.ac.ucl.excites.sapelli.storage.queries.constraints.ConstraintVisitor)
	 */
	@Override
	public void accept(ConstraintVisitor visitor)
	{
		visitor.visit(this);
	}
	
	/**
	 * @return the negatedConstraint
	 */
	public Constraint getNegatedConstraint()
	{
		return negatedConstraint;
	}

}
