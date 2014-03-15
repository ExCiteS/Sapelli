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
	
	private static final long serialVersionUID = 1L;

	public static final int MIN_POINTS = 2;
	
	public Line()
	{
		super(MIN_POINTS);
	}
		
	public Line(int initialCapacity)
	{
		super(initialCapacity);
	}
	
}
