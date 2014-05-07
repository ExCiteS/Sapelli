/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries.constraints;

import java.util.ArrayList;
import java.util.List;

/**
 * @author mstevens
 *
 */
public abstract class CompositeConstraint extends Constraint
{

	protected final List<Constraint> constraints;

	/**
	 * @param constraints
	 */
	public CompositeConstraint(Constraint... constraints)
	{
		this.constraints = new ArrayList<Constraint>();
		if(constraints != null)
			for(Constraint c : constraints)
				addConstraint(c);
	}
	
	public void addConstraint(Constraint constraint)
	{
		if(constraint == null)
			return;
		if(this.getClass().isInstance(constraint) && isAssociative())
			// Flatten instance of same CompositeConstraint subclass when associative:
			for(Constraint subConstraint : ((CompositeConstraint) constraint).constraints)
				this.constraints.add(subConstraint);
		else
			this.constraints.add(constraint);
	}

	public List<Constraint> getSubConstraints()
	{
		return constraints;
	}

	public boolean hasSubConstraints()
	{
		return !constraints.isEmpty();
	}
	
	protected abstract boolean isAssociative();

}
