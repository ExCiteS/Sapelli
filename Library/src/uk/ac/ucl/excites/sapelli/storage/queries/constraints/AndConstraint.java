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
			{
				if(c instanceof AndConstraint)
					// Flatten nested ANDs (AND is associative):
					for(Constraint sc : ((AndConstraint) c).constraints)
						this.constraints.add(sc);
				else
					this.constraints.add(c);
			}
	}
	
	public void addConstraint(Constraint constraint)
	{
		constraints.add(constraint);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.queries.Filter#_select(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	protected boolean isValid(Record record)
	{
		for(Constraint f : constraints)
			if(!f.isValid(record))
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
