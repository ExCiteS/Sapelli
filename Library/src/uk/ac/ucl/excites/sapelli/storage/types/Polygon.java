/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.types;

import java.util.ArrayList;


/**
 * A polygon, implemented as a List of {@link Location}s
 * 
 * TODO check for duplicate points?
 * TODO close shape method?
 * 
 * @author mstevens
 */
public class Polygon extends ArrayList<Location>
{
	
	private static final long serialVersionUID = 1L;

	public static final int MIN_POINTS = 3;
	
	public Polygon()
	{
		super(MIN_POINTS);
	}
	
	public Polygon(int initialCapacity)
	{
		super(initialCapacity);
	}
	
}
