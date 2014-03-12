/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model.columns;

import uk.ac.ucl.excites.sapelli.storage.model.RecordColumn;
import uk.ac.ucl.excites.sapelli.storage.types.Orientation;

/**
 * @author mstevens
 *
 */
public class OrientationColumn extends RecordColumn<Orientation>
{
	
	public OrientationColumn(String name, boolean optional, boolean storeAzimuth, boolean storePitch, boolean storeRoll)
	{
		super(Orientation.class, name, Orientation.SCHEMA, optional);
		if(!storeAzimuth)
			addSkipColumn(Orientation.COLUMN_AZIMUTH);
		if(!storePitch)
			addSkipColumn(Orientation.COLUMN_PITCH);
		if(!storeRoll)
			addSkipColumn(Orientation.COLUMN_ROLL);
	}
	
	@Override
	protected Orientation copy(Orientation value)
	{
		return new Orientation(value);
	}

	@Override
	protected Orientation getNewRecord()
	{
		return new Orientation();
	}
	
	@Override
	public OrientationColumn copy()
	{
		return new OrientationColumn(name, optional, !skipColumns.contains(Orientation.COLUMN_AZIMUTH), !skipColumns.contains(Orientation.COLUMN_PITCH), !skipColumns.contains(Orientation.COLUMN_ROLL));
	}
	
}
