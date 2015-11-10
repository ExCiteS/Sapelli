/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.storage.visitors;

import java.util.Collections;
import java.util.Set;
import java.util.Stack;

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ColumnSet;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSet;
import uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;

/**
 * A {@link SimpleColumnVisitor} which can traverse all (sub)columns of a Schema and which has
 * a single abstract {@link #visit(ColumnPointer)} method (used for all column types).
 * The class keeps of stack of (parent) columns, which always includes the one being visited at the top.
 * 
 * @author mstevens
 */
public abstract class SimpleSchemaTraverser extends SimpleColumnVisitor
{

	private final Stack<Column<?>> columnStack = new Stack<Column<?>>();
	
	protected final void traverse(Schema schema)
	{
		traverse(schema, Collections.<Column<?>> emptySet());
	}
	
	protected final void traverse(Schema schema, Set<? extends Column<?>> skipColumns)
	{
		columnStack.clear();
		schema.accept(this, skipColumns);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor#enter(uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn)
	 */
	@Override
	public <VS extends ValueSet<CS>, CS extends ColumnSet> void enter(ValueSetColumn<VS, CS> valueSetCol)
	{
		columnStack.push(valueSetCol);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor#leave(uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn)
	 */
	@Override
	public <VS extends ValueSet<CS>, CS extends ColumnSet> void leave(ValueSetColumn<VS, CS> valueSetCol)
	{
		if(columnStack.peek() != valueSetCol)
			throw new IllegalStateException("Invalid column stack state!");
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
	protected final <T> void visit(Column<T> column)
	{
		columnStack.push(column);
		visit(getColumnPointer());
		columnStack.pop();
	}
	
	protected final ColumnPointer<?> getColumnPointer()
	{
		return ColumnPointer.FromList(columnStack);
	}
	
	/**
	 * Visit leaf column, indicated by a ColumnPointer
	 * 
	 * @param leafColumnPointer
	 */
	public abstract void visit(ColumnPointer<?> leafColumnPointer);

}
