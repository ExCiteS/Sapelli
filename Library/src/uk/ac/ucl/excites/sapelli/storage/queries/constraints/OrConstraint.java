/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries.constraints;

import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * Disjunction of Constraints.
 * 
 * @author mstevens
 */
public class OrConstraint extends CompositeConstraint
{

	public OrConstraint(Constraint... constraints)
	{
		super(constraints);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#_isValid(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	protected boolean _isValid(Record record)
	{
		for(Constraint subConstraint : constraints)
			if(subConstraint._isValid(record))
				return true;
		return !hasSubConstraints(); // if we do not have subConstraints then any record is valid, if we *do* have subconstraints then reaching this line means none of them caused us to return true above, so return false.
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
	protected boolean isAssociative()
	{
		return true;
	}

}
