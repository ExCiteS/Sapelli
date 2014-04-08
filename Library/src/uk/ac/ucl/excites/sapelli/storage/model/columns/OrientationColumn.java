/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model.columns;

import uk.ac.ucl.excites.sapelli.storage.model.RecordColumn;
import uk.ac.ucl.excites.sapelli.storage.types.Orientation;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * A column for {@link Orientation}s, implemented as a {@link RecordColumn} subclass.
 * 
 * @author mstevens
 */
public class OrientationColumn extends RecordColumn<Orientation>
{
	
	static private final long serialVersionUID = 2L;
	
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
	public Orientation getNewRecord()
	{
		return new Orientation();
	}
	
	public boolean isStoreAzimuth()
	{
		return !skipColumns.contains(Orientation.COLUMN_AZIMUTH);
	}
	
	public boolean isStorePitch()
	{
		return !skipColumns.contains(Orientation.COLUMN_PITCH);
	}
	
	public boolean isStoreRoll()
	{
		return !skipColumns.contains(Orientation.COLUMN_ROLL);
	}
	
	@Override
	public OrientationColumn copy()
	{
		return new OrientationColumn(name, optional, isStoreAzimuth(), isStorePitch(), isStoreRoll());
	}
	
	@Override
	public void accept(ColumnVisitor visitor)
	{
		if(visitor.allowOrientationSelfTraversal())
			super.accept(visitor, !visitor.skipNonBinaryStoredOrientationColumns());
		else
			visitor.visit(this);
	}
	
}
