/**
 * 
 */
package uk.ac.ucl.excites.collector.model;

import uk.ac.ucl.excites.storage.model.Schema;

/**
 * @author Michalis Vitos
 *
 */
public class Photo extends Field
{
	private int min;
	private int max;
	private Choice disableChoice;
	
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
	
	/**
	 * @return the min
	 */
	public int getMin()
	{
		return min;
	}
	/**
	 * @param min the min to set
	 */
	public void setMin(int min)
	{
		this.min = min;
	}
	/**
	 * @return the max
	 */
	public int getMax()
	{
		return max;
	}
	/**
	 * @param max the max to set
	 */
	public void setMax(int max)
	{
		this.max = max;
	}
	/**
	 * @return the disableChoice
	 */
	public Choice getDisableChoice()
	{
		return disableChoice;
	}
	/**
	 * @param disableChoice the disableChoice to set
	 */
	public void setDisableChoice(Choice disableChoice)
	{
		this.disableChoice = disableChoice;
	}

	@Override
	protected void _addColumns(Schema schema)
	{
		// TODO Auto-generated method stub
		
	}
	
}
