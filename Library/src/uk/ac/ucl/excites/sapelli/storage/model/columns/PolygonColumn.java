/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model.columns;

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ListColumn;
import uk.ac.ucl.excites.sapelli.storage.types.Location;
import uk.ac.ucl.excites.sapelli.storage.types.Polygon;

/**
 * @author mstevens
 *
 */
public class PolygonColumn extends ListColumn<Location>
{
	
	static public final int SIZE_FIELD_BITS = 16;

	public PolygonColumn(String name, boolean optional, boolean doublePrecision, boolean storeAltitude, boolean storeAccuracy, boolean storeTime, boolean storeProvider)
	{
		this(name, new LocationColumn("Point", false, doublePrecision, storeAltitude, false, false, storeAccuracy, storeTime, storeProvider), optional);
	}
	
	private PolygonColumn(String name, Column<Location> locationCol, boolean optional)
	{
		super(name, locationCol, optional, Polygon.MIN_POINTS, GetMaxLengthForSizeFieldSize(Polygon.MIN_POINTS, SIZE_FIELD_BITS));
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

}
