/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
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

package uk.ac.ucl.excites.sapelli.storage.types;

import uk.ac.ucl.excites.sapelli.storage.model.ColumnSet;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * A column for {@link Orientation}s, implemented as a {@link ValueSetColumn} subclass.
 * 
 * @author mstevens
 */
public class OrientationColumn extends ValueSetColumn<Orientation, ColumnSet>
{
	
	static private final long serialVersionUID = 2L;
	
	/**
	 * @param name
	 * @param optional
	 * @param storeAzimuth
	 * @param storePitch
	 * @param storeRoll
	 */
	public OrientationColumn(String name, boolean optional, boolean storeAzimuth, boolean storePitch, boolean storeRoll)
	{
		this(name, optional, storeAzimuth, storePitch, storeRoll, null);
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param storeAzimuth
	 * @param storePitch
	 * @param storeRoll
	 * @param defaultValue
	 */
	public OrientationColumn(String name, boolean optional, boolean storeAzimuth, boolean storePitch, boolean storeRoll, Orientation defaultValue)
	{
		super(name, Orientation.COLUMN_SET, optional, defaultValue);
		if(!storeAzimuth)
			skipColumn(Orientation.COLUMN_AZIMUTH);
		if(!storePitch)
			skipColumn(Orientation.COLUMN_PITCH);
		if(!storeRoll)
			skipColumn(Orientation.COLUMN_ROLL);
	}
	
	@Override
	protected Orientation copy(Orientation value)
	{
		return new Orientation(value);
	}

	@Override
	public Orientation getNewValueSet()
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
	protected OrientationColumn createCopy()
	{
		return new OrientationColumn(name, optional, isStoreAzimuth(), isStorePitch(), isStoreRoll(), defaultValue);
	}
	
	@Override
	public void accept(ColumnVisitor visitor)
	{
		if(visitor.splitOrientationTraversal())
			super.accept(visitor, !visitor.skipNonBinarySerialisedOrientationSubColumns()); // visit as ValueSetColumn: enter event, visit or each subcolumn, leave event
		else
			visitor.visit(this); // visit as OrientationColumn (as a single whole)
	}

	@Override
	public Class<Orientation> getType()
	{
		return Orientation.class;
	}
	
}
