/**
 * 
 */
package uk.ac.ucl.excites.collector.model;

import uk.ac.ucl.excites.storage.model.Schema;

/**
 * @author mstevens
 *
 */
public class MediaAttachment extends Field
{

	protected int min;
	protected int max;
	protected Choice disableChoice;

	@Override
	protected void _addColumns(Schema schema)
	{
		// TODO Auto-generated method stub
		
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

}
