/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model.columns;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.ColumnVisitor;
import uk.ac.ucl.excites.sapelli.storage.model.ListColumn;

/**
 * @author mstevens
 *
 */
public class IntegerListColumn extends ListColumn<Long>
{

	public IntegerListColumn(String name, IntegerColumn singleColumn, boolean optional, int minLength, int maxLength)
	{
		super(name, singleColumn, optional, minLength, maxLength);
	}
	
	@Override
	public IntegerListColumn copy()
	{
		return new IntegerListColumn(name, (IntegerColumn) singleColumn.copy(), optional, getMinimumLength(), getMaximumLength());
	}

	@Override
	protected List<Long> getNewList(int minimumCapacity)
	{
		return new ArrayList<Long>(minimumCapacity);
	}

	@Override
	public void accept(ColumnVisitor visitor)
	{
		visitor.visit(this);
	}

}
