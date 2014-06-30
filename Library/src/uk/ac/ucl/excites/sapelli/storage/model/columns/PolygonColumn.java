/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model.columns;

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ListColumn;
import uk.ac.ucl.excites.sapelli.storage.types.Location;
import uk.ac.ucl.excites.sapelli.storage.types.Polygon;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * A column for {@link Polygon}s, implemented as a {@link ListColumn} subclass.
 * 
 * @author mstevens
 */
public class PolygonColumn extends ListColumn<Polygon, Location>
{
	
	static private final long serialVersionUID = 2L;
	
	static public final int SIZE_FIELD_BITS = 16;

	public PolygonColumn(String name, boolean optional, boolean doublePrecision, boolean storeAltitude, boolean storeAccuracy, boolean storeTime, boolean storeProvider)
	{
		this(name, new LocationColumn("Point", false, doublePrecision, storeAltitude, false, false, storeAccuracy, storeTime, storeProvider), optional);
	}
	
	private PolygonColumn(String name, Column<Location> locationCol, boolean optional)
	{
		super(name, locationCol, optional, 0, GetMaxLengthForSizeFieldSize(0, SIZE_FIELD_BITS));
	}
	
	@Override
	public PolygonColumn copy()
	{
		return new PolygonColumn(name, singleColumn.copy(), optional);
	}

	@Override
	protected Polygon getNewList(int minimumCapacity)
	{
		return new Polygon(minimumCapacity);
	}
	
	@Override
	public String getTypeString()
	{
		return Polygon.class.getSimpleName();
	}

	@Override
	public void accept(ColumnVisitor visitor)
	{
		visitor.visit(this);
	}

}
