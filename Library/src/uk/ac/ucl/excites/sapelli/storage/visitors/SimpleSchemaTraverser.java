/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.visitors;

import java.util.Stack;

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.RecordColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;

/**
 * @author mstevens
 *
 */
public abstract class SimpleSchemaTraverser extends SimpleColumnVisitor
{

	private final Stack<Column<?>> columnStack = new Stack<Column<?>>();
	
	public void traverse(Schema schema)
	{
		columnStack.clear();
		schema.accept(this);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnVisitor#enter(uk.ac.ucl.excites.sapelli.storage.model.RecordColumn)
	 */
	@Override
	public void enter(RecordColumn<?> recordCol)
	{
		columnStack.push(recordCol);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnVisitor#leave(uk.ac.ucl.excites.sapelli.storage.model.RecordColumn)
	 */
	@Override
	public void leave(RecordColumn<?> recordCol)
	{
		columnStack.pop();
	}
	
	/**
	 * Visit method for VirtualColumns. We treat them like any other column (i.e. we do *not* visit their target column directly). 
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn)
	 */
	@Override
	public <VT, ST> void visit(VirtualColumn<VT, ST> virtCol)
	{
		visit((Column<VT>) virtCol);
	}
	
	@Override
	public <T> void visit(Column<T> column)
	{
		columnStack.push(column);
		
		visit(getColumnPointer());
		
		columnStack.pop();
	}
	
	protected ColumnPointer getColumnPointer()
	{
		return new ColumnPointer(columnStack);
	}
	
	/**
	 * Visit leaf column, indicated by a ColumnPointer
	 * 
	 * @param leafColumnPointer
	 */
	public abstract void visit(ColumnPointer leafColumnPointer);

}
