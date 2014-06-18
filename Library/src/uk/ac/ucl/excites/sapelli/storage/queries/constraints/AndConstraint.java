/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries.constraints;

import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * Conjuction of Constraints
 * 
 * @author mstevens
 */
public class AndConstraint extends CompositeConstraint
{

	public AndConstraint(Constraint... constraints)
	{
		super(constraints);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#_isValid(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	protected boolean _isValid(Record record)
	{
		if(hasSubConstraints())
			for(Constraint subConstraint : constraints)
				if(!subConstraint._isValid(record))
					return false;
		return true; // also if we didn't have any subConstraints
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
	
	@Override
	public boolean equals(Object obj)
	{
		if(this == obj)
			return true; // references to same object
		if(obj instanceof AndConstraint)
			return super.equals(obj); // CompositeConstraint#equals()
		return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // CompositeConstraint#hashCode()
		hash = 31 * hash + "AND".hashCode(); // to differentiate from OrConstraint
		return hash;
	}

}
