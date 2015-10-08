/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2014 University College London - ExCiteS group
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 */

package uk.ac.ucl.excites.sapelli.storage.model.columns;

import uk.ac.ucl.excites.sapelli.storage.model.ColumnSet;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn;
import uk.ac.ucl.excites.sapelli.storage.types.Orientation;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * A column for {@link Orientation}s, implemented as a {@link ValueSetColumn} subclass.
 * 
 * @author mstevens
 */
public class OrientationColumn extends ValueSetColumn<Orientation, ColumnSet>
{
	
	static private final long serialVersionUID = 2L;
	
	public OrientationColumn(String name, boolean optional, boolean storeAzimuth, boolean storePitch, boolean storeRoll)
	{
		super(name, Orientation.COLUMN_SET, optional);
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
		return !isColumnSkipped(Orientation.COLUMN_AZIMUTH);
	}
	
	public boolean isStorePitch()
	{
		return !isColumnSkipped(Orientation.COLUMN_PITCH);
	}
	
	public boolean isStoreRoll()
	{
		return !isColumnSkipped(Orientation.COLUMN_ROLL);
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
			super.accept(visitor, !visitor.skipNonBinarySerialisedOrientationSubColumns());
		else
			visitor.visit(this);
	}

	@Override
	public Class<Orientation> getType()
	{
		return Orientation.class;
	}
	
}
