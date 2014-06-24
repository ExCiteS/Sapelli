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
		if(!hasSubConstraints())
			return true; // if we do not have subConstraints then any record is valid.
		for(Constraint subConstraint : constraints)
			if(subConstraint._isValid(record))
				return true;
		return false; // if we *do* have subconstraints then reaching this line means none of them caused us to return true above, so return false.
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
		if(obj instanceof OrConstraint)
			return super.equals(obj); // CompositeConstraint#equals()
		return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode(); // CompositeConstraint#hashCode()
		hash = 31 * hash + "OR".hashCode(); // to differentiate from AndConstraint
		return hash;
	}

}
