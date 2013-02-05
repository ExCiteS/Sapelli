/**
 * 
 */
package uk.ac.ucl.excites.collector.model;

import uk.ac.ucl.excites.storage.model.IntegerColumn;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;

/**
 * @author mstevens
 *
 */
public class MediaAttachment extends Field
{

	static public final int DEFAULT_MIN = 0;
	static public final int DEFAULT_MAX = 255; //column will use 1 byte
	
	protected int min;
	protected int max;
	protected Choice disableChoice;

	public MediaAttachment(String id)
	{
		this(id, DEFAULT_MIN, DEFAULT_MAX, null); //Use on byte --> up to 255 items
	}

	/**
	 * @param id
	 * @param min
	 * @param max
	 * @param disableChoice
	 */
	public MediaAttachment(String id, int min, int max, Choice disableChoice)
	{
		super(id);
		if(id == null)
			throw new NullPointerException("ID of top-level field cannot be null");
		setMinMax(min, max);
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
	 * @return the max
	 */
	public int getMax()
	{
		return max;
	}

	/**
	 * @param min the min to set
	 * @param max the max to set
	 */
	public void setMinMax(int min, int max)
	{
		if(max < 0 || min < 0 || min > max)
			throw new IllegalArgumentException("Min and max must be > 0 and min <= max! Supplied values are min = " + min + "; max = " + max + ".");
		this.min = min;
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
	public void addColumns(Schema schema)
	{
		schema.addColumn(new IntegerColumn(id, false, min, max));
	}
	
	@Override
	public void storeValues(Record record)
	{
		int currentNumberOfItems = (Integer) record.get(id);
		
		//TODO 
	}

}
