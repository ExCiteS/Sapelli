/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.types;

import java.util.ArrayList;

/**
 * A polygon, implemented as a List of {@link Location}s
 * 
 * @author mstevens
 */
public class Line extends ArrayList<Location>
{
	
	static private final long serialVersionUID = 2L;
	
	public static final int MIN_POINTS = 2;
	
	public Line()
	{
		super(MIN_POINTS);
	}
		
	public Line(int initialCapacity)
	{
		super(initialCapacity);
	}
	
	@Override
	public boolean add(Location point)
	{
		if(point != null)
			return super.add(point);
		else
			throw new NullPointerException("Cannot add null point");
	}
	
	public boolean isValid()
	{
		return size() >= MIN_POINTS; // TODO check uniqueness of points, etc.
	}
	
}
