package uk.ac.ucl.excites.sapelli.collector.model.fields;

import uk.ac.ucl.excites.sapelli.collector.control.Controller;
import uk.ac.ucl.excites.sapelli.collector.model.CollectorRecord;
import uk.ac.ucl.excites.sapelli.collector.model.Field;
import uk.ac.ucl.excites.sapelli.collector.model.Form;
import uk.ac.ucl.excites.sapelli.collector.ui.CollectorUI;
import uk.ac.ucl.excites.sapelli.collector.ui.fields.OrientationUI;
import uk.ac.ucl.excites.sapelli.storage.model.columns.OrientationColumn;
import uk.ac.ucl.excites.sapelli.storage.types.Orientation;

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
	
	/**
	 * @param form the form the field belongs to
	 * @param id the id of the field, should not be null
	 * @param caption the caption of the field, may be null (in which case the id is used as the caption)
	 */
	public OrientationField(Form form, String id, String caption)
	{
		super(form, id, caption);
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

	public void storeOrientation(CollectorRecord record, Orientation orientation)
	{
		((OrientationColumn) form.getColumnFor(this)).storeValue(record, orientation);
	}

	@Override
	public boolean enter(Controller controller, boolean withPage)
	{
		if(!withPage)
			return controller.enterOrientationField(this);
		return true;
	}

	@Override
	public <V, UI extends CollectorUI<V, UI>> OrientationUI<V, UI> createUI(UI collectorUI)
	{
		return collectorUI.createOrientationUI(this);
	}

}
