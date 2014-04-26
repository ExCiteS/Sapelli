/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries.constraints;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.Record;

/**
 * Conjuction of Constraints
 * 
 * @author mstevens
 */
public class AndConstraint extends Constraint
{

	private final List<Constraint> constraints;
	
	public AndConstraint(Constraint... constraints)
	{
		this.constraints = new ArrayList<Constraint>();
		if(constraints != null)
			for(Constraint c : constraints)
				addConstraint(c);
	}
	
	public void addConstraint(Constraint constraint)
	{
		if(constraint instanceof AndConstraint)
			// Flatten nested ANDs (AND is associative):
			for(Constraint subConstraint : ((AndConstraint) constraint).constraints)
				this.constraints.add(subConstraint);
		else
			this.constraints.add(constraint);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint#_isValid(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	protected boolean _isValid(Record record)
	{
		for(Constraint subConstraint : constraints)
			if(!subConstraint._isValid(record))
				return false;
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
	
	public List<Constraint> getSubConstraints()
	{
		return constraints;
	}

}
