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
	
	/**
	 * @param constraintToNegate
	 * @return
	 */
	static Constraint Negate(Constraint constraintToNegate)
	{
		if(constraintToNegate instanceof NotConstraint)
			// Avoid double-negations:
			return ((NotConstraint) constraintToNegate).negatedConstraint;
		else
			return new NotConstraint(constraintToNegate);
	}

	private final Constraint negatedConstraint;
	
	private NotConstraint(Constraint negatedConstraint)
	{
		if(negatedConstraint == null)
			throw new NullPointerException("negatedConstraint cannot be null!");
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
