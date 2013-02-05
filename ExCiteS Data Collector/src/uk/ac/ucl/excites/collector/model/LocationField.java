/**
 * 
 */
package uk.ac.ucl.excites.collector.model;

import uk.ac.ucl.excites.storage.model.FloatColumn;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.model.Schema;

/**
 * @author mstevens
 *
 */
public class LocationField extends Field
{

	public static final int TYPE_GPS = 1;
	//...

	private int type;
	private float accuracy;
	private int timeoutS;
	
	public LocationField(String id)
	{
		super(id);
	}
	
	/**
	 * @return the type
	 */
	public int getType()
	{
		return type;
	}

	/**
	 * @param type the type to set
	 */
	public void setType(int type)
	{
		this.type = type;
	}

	/**
	 * @return the accuracy
	 */
	public float getAccuracy()
	{
		return accuracy;
	}

	/**
	 * @param accuracy the accuracy to set
	 */
	public void setAccuracy(float accuracy)
	{
		this.accuracy = accuracy;
	}

	/**
	 * @return the timeoutS
	 */
	public int getTimeoutS()
	{
		return timeoutS;
	}

	/**
	 * @param timeoutS the timeoutS to set
	 */
	public void setTimeoutS(int timeoutS)
	{
		this.timeoutS = timeoutS;
	}

	@Override
	public void addColumns(Schema schema)
	{
		schema.addColumn(new FloatColumn(id + "_LAT", true));
		schema.addColumn(new FloatColumn(id + "_LON", true));
		//TODO when we have support for composite columns LON & LAT should be combined and share 1 optional bit (or not be optional at all depending on the project?)
		schema.addColumn(new FloatColumn(id + "_ALT", true)); //this should be dependent on project file
		schema.addColumn(new FloatColumn(id + "_ACC", true));
	}

	@Override
	public void storeValues(Record record)
	{
		// TODO Auto-generated method stub
		
	}
	
}
