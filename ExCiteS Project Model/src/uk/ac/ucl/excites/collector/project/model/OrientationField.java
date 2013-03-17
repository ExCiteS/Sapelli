package uk.ac.ucl.excites.collector.project.model;

import uk.ac.ucl.excites.collector.project.ui.CollectorUI;
import uk.ac.ucl.excites.storage.model.OrientationColumn;
import uk.ac.ucl.excites.storage.model.Record;
import uk.ac.ucl.excites.storage.types.Orientation;


/**
 * @author mstevens
 *
 */
public class OrientationField extends Field
{
	
	//Statics
	static public final boolean DEFAULT_STORE_AZIMUTH = true; 
	static public final boolean DEFAULT_STORE_PITCH = true;
	static public final boolean DEFAULT_STORE_ROLL = true;
	
	//Dynamics:
	private boolean storeAzimuth = DEFAULT_STORE_AZIMUTH;
	private boolean storePitch = DEFAULT_STORE_PITCH;
	private boolean storeRoll = DEFAULT_STORE_ROLL;
	
	public OrientationField(Form form, String id)
	{
		super(form, id);
	}

	/**
	 * @return the storeAzimuth
	 */
	public boolean isStoreAzimuth()
	{
		return storeAzimuth;
	}

	/**
	 * @param storeAzimuth the storeAzimuth to set
	 */
	public void setStoreAzimuth(boolean storeAzimuth)
	{
		this.storeAzimuth = storeAzimuth;
	}

	/**
	 * @return the storePitch
	 */
	public boolean isStorePitch()
	{
		return storePitch;
	}

	/**
	 * @param storePitch the storePitch to set
	 */
	public void setStorePitch(boolean storePitch)
	{
		this.storePitch = storePitch;
	}

	/**
	 * @return the storeRoll
	 */
	public boolean isStoreRoll()
	{
		return storeRoll;
	}

	/**
	 * @param storeRoll the storeRoll to set
	 */
	public void setStoreRoll(boolean storeRoll)
	{
		this.storeRoll = storeRoll;
	}

	@Override
	protected OrientationColumn createColumn()
	{
		return new OrientationColumn(id, (optional != Optionalness.NEVER), storeAzimuth, storePitch, storeRoll);
	}

	public void storeValue(Record record, Orientation orientation)
	{
		((OrientationColumn) column).storeValue(record, orientation);
	}
	
	@Override
	public void setIn(CollectorUI fv)
	{
		fv.setOrientation(this);
	}	

}
