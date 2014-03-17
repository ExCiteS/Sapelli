/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model.columns;

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ColumnVisitor;
import uk.ac.ucl.excites.sapelli.storage.model.ListColumn;
import uk.ac.ucl.excites.sapelli.storage.types.Line;
import uk.ac.ucl.excites.sapelli.storage.types.Location;

/**
 * A column for {@link Line}s, implemented as a {@link ListColumn} subclass.
 * 
 * @author mstevens
 */
public class LineColumn extends ListColumn<Location>
{
	
	static public final int SIZE_FIELD_BITS = 16;

	public LineColumn(String name, boolean optional, boolean doublePrecision, boolean storeAltitude, boolean storeAccuracy, boolean storeTime, boolean storeProvider)
	{
		this(name, new LocationColumn("Point", false, doublePrecision, storeAltitude, false, false, storeAccuracy, storeTime, storeProvider), optional);
	}
	
	private LineColumn(String name, Column<Location> locationCol, boolean optional)
	{
		super(name, locationCol, optional, Line.MIN_POINTS, GetMaxLengthForSizeFieldSize(Line.MIN_POINTS, SIZE_FIELD_BITS));
	}
	
	@Override
	public LineColumn copy()
	{
		return new LineColumn(name, singleColumn.copy(), optional);
	}

	@Override
	protected Line getNewList(int minimumCapacity)
	{
		return new Line(minimumCapacity);
	}
	
	@Override
	public String getTypeString()
	{
		return Line.class.getSimpleName();
	}

	@Override
	public void accept(ColumnVisitor visitor)
	{
		visitor.visit(this);
	}

}
