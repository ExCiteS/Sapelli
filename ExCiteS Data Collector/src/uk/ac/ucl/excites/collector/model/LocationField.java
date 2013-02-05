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
	
	//Statics
	public static final int TYPE_GPS = 1;
	public static final int TYPE_NETWORK = 2;
	public static final int TYPE_DEFAULT = TYPE_GPS;
	
	public static final int DEFAULT_TIMEOUT_S = 300;
	
	//Dynamics
	private int type;
	private int timeoutS;
	
	public LocationField(String id)
	{
		super(id);
		if(id == null)
			throw new NullPointerException("ID of top-level field cannot be null");
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
	 * This is the time to wait for coordinates while already on the waiting screen, time spent before is not counted
	 * 
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
