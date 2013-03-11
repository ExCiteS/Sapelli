package uk.ac.ucl.excites.collector.project.model;

import uk.ac.ucl.excites.collector.project.ui.CollectorUI;
import uk.ac.ucl.excites.storage.model.OrientationColumn;


/**
 * @author mstevens
 *
 */
public class OrientationField extends Field
{

	//Statics:
	static public final char X_AXIS = 'X';
	static public final char Y_AXIS = 'Y';
	static public final char Z_AXIS = 'Z';
	
	//Dynamics:
	private boolean storeX = false;
	private boolean storeY = false;
	private boolean storeZ = false;
	
	public OrientationField(String id, String axes)
	{
		super(id);
		if(axes != null)
		{
			for(char c : axes.trim().toUpperCase().toCharArray())
				switch(c)
				{
					case X_AXIS : storeX = true; break;
					case Y_AXIS : storeY = true; break;
					case Z_AXIS : storeZ = true; break;
				}
		}
		//If none are stored (axes string is probably null, empty or invalid), then store all 3 axes:
		if(!storeX && !storeY && !storeZ)
		{
			storeX = true;
			storeY = true;
			storeZ = true;
		}
	}

	@Override
	protected OrientationColumn createColumn()
	{
		return new OrientationColumn(id, true /*always optional (in case the sensor doesn't work)*/, storeX, storeY, storeZ);
	}

	@Override
	public void setIn(CollectorUI fv)
	{
		fv.setOrientation(this);
	}	

}
