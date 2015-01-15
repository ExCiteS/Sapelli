package uk.ac.ucl.excites.sapelli.storage.model.columns;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ListColumn;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

public class BooleanListColumn extends ListColumn<List<Boolean>, Boolean>
{

	private static final long serialVersionUID = 1L;

	public BooleanListColumn(String name, boolean optional, int minLength, int maxLength)
	{
		super(name, new BooleanColumn(Boolean.class.getSimpleName(), false), optional, minLength, maxLength);
	}

	@Override
	protected List<Boolean> getNewList(int minimumCapacity)
	{
		return new ArrayList<Boolean>(minimumCapacity);
	}

	@Override
	public Column<List<Boolean>> copy()
	{
		return new BooleanListColumn(name, optional, getMinimumLength(), getMaximumLength());
	}

	@Override
	public void accept(ColumnVisitor visitor)
	{
		visitor.visit(this);		
	}

}
