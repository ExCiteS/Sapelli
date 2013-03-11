/**
 * 
 */
package uk.ac.ucl.excites.collector.project.model;

import uk.ac.ucl.excites.storage.model.IntegerColumn;
import uk.ac.ucl.excites.storage.model.Record;

/**
 * @author mstevens
 *
 */
public abstract class MediaAttachment extends Field
{

	//static public final int DEFAULT_MIN = 0;
	static public final int DEFAULT_MAX = 255; //column will use 1 byte
	
	//protected int min;
	protected int max;
	protected Choice disableChoice;

	public MediaAttachment(String id)
	{
		this(id, /*DEFAULT_MIN,*/ DEFAULT_MAX, null); //Use on byte --> up to 255 items
	}

	/**
	 * @param id
	 * @param max
	 * @param disableChoice
	 */
	public MediaAttachment(String id, /*int min,*/ int max, Choice disableChoice)
	{
		super(id);
		if(id == null)
			throw new NullPointerException("ID of top-level field cannot be null");
		setMax(max); //setMinMax(min, max);
		this.disableChoice = disableChoice;
	}

//	/**
//	 * @return the min
//	 */
//	public int getMin()
//	{
//		return min;
//	}
	
	/**
	 * @return the max
	 */
	public int getMax()
	{
		return max;
	}

//	/**
//	 * @param min the min to set
//	 * @param max the max to set
//	 */
//	public void setMinMax(int min, int max)
//	{
//		if(max < 1 || min < 0 || min > max)
//			throw new IllegalArgumentException("Min must be >= 0, max must be >= 1, and min <= max! Supplied values are min = " + min + "; max = " + max + ".");
//		this.min = min;
//		this.max = max;
//	}
	
	/**
	 * @param max the max to set
	 */
	public void setMax(int max)
	{
		if(max < 1)
			throw new IllegalArgumentException("Max must be >= 1, supplied value is " + max + ".");
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
	protected IntegerColumn createColumn()
	{
		return new IntegerColumn(id, (optional != Optionalness.NEVER), (optional != Optionalness.NEVER ? 0 : 1), max);
	}
	
	public int getCount(Record record)
	{
		Long currentCount = ((IntegerColumn) column).retrieveValue(record);
		if(currentCount == null)
			return 0;
		return currentCount.intValue();
	}
	
	public boolean isMaxReached(Record record)
	{
		return (getCount(record) >= max);
	}
	
	public void incrementCount(Record record)
	{
		int currentCount = getCount(record);	
		if(currentCount >= max)
			throw new IllegalStateException("Maximum # of attachments (" + max + ") reached.");
		((IntegerColumn) column).storeValue(record, Long.valueOf(++currentCount));
	}

}
