/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries.constraints;

/**
 * Visitor interface for examining Constraints
 * 
 * @author mstevens
 */
public interface ConstraintVisitor
{
	
	public void visit(AndConstraint andConstr);
	
	public void visit(OrConstraint orConstr);
	
	public void visit(NotConstraint notConstr);

	public void visit(EqualityConstraint equalityQuery);
		
	public void visit(RuleConstraint ruleQuery);

}
