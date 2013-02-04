/**
 * 
 */
package uk.ac.ucl.excites.collector.model;

/**
 * @author Michalis Vitos
 *
 */
public class Photo extends MediaAttachment
{
	public Photo()
	{
		// Set the min to 0 and the max to the maximum value of an Integer
		this(0, Integer.MAX_VALUE, null);
	}
	
	/**
	 * @param min
	 * @param max
	 * @param disableChoice
	 */
	public Photo(int min, int max, Choice disableChoice)
	{
		this.min = min;
		this.max = max;
		this.disableChoice = disableChoice;
	}
	
}
