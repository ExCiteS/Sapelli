/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.visitors;

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.DateTimeColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.FloatColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.LineColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.LocationColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.OrientationColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.PolygonColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;

/**
 * A {@link ColumnVisitor} that treats all Column types the same
 * 
 * @author mstevens
 */
public abstract class SimpleColumnVisitor implements ColumnVisitor
{

	/**
	 * One method to deal with all types of columns
	 * 
	 * @param leafColumn
	 */
	public abstract <T> void visit(Column<T> Column);
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn)
	 */
	@Override
	public void visit(BooleanColumn boolCol)
	{
		this.visit((Column<?>) boolCol);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.DateTimeColumn)
	 */
	@Override
	public void visit(DateTimeColumn dateTimeCol)
	{
		this.visit((Column<?>) dateTimeCol);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.FloatColumn)
	 */
	@Override
	public void visit(FloatColumn floatCol)
	{
		this.visit((Column<?>) floatCol);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn)
	 */
	@Override
	public void visit(ForeignKeyColumn foreignKeyCol)
	{
		this.visit((Column<?>) foreignKeyCol);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn)
	 */
	@Override
	public void visit(IntegerColumn intCol)
	{
		this.visit((Column<?>) intCol);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerListColumn)
	 */
	@Override
	public void visit(IntegerListColumn intListCol)
	{
		this.visit((Column<?>) intListCol);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.LineColumn)
	 */
	@Override
	public void visit(LineColumn lineCol)
	{
		this.visit((Column<?>) lineCol);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.LocationColumn)
	 */
	@Override
	public void visit(LocationColumn locCol)
	{
		this.visit((Column<?>) locCol);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.OrientationColumn)
	 */
	@Override
	public void visit(OrientationColumn orCol)
	{
		this.visit((Column<?>) orCol);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.PolygonColumn)
	 */
	@Override
	public void visit(PolygonColumn polyCol)
	{
		this.visit((Column<?>) polyCol);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.util.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn)
	 */
	@Override
	public void visit(StringColumn stringCol)
	{
		this.visit((Column<?>) stringCol);
	}

}
