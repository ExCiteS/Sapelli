/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.queries;

/**
 * @author mstevens
 *
 */
public interface QueryBuilder
{

	public void visit(SchemaSelectionQuery schemaQuery);
	
	public void visit(EqualitySelectionQuery equalityQuery);
	
	public void visit(CompositeSelectionQuery composityQuery);
	
	public boolean isCompsitesSelfTraversalAllowed();

}
