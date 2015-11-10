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

package uk.ac.ucl.excites.sapelli.storage.types;

import java.util.Collection;

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ListColumn;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * A column for {@link Polygon}s, implemented as a {@link ListColumn} subclass.
 * 
 * @author mstevens
 */
public class PolygonColumn extends ListColumn<Polygon, Location>
{
	
	static private final long serialVersionUID = 2L;
	
	static public final int SIZE_FIELD_BITS = 16;

	public PolygonColumn(String name, boolean optional, boolean doublePrecision, boolean storeAltitude, boolean storeAccuracy, boolean storeTime, boolean storeProvider)
	{
		this(name, optional, doublePrecision, storeAltitude, storeAccuracy, storeTime, storeProvider, null);
	}
	
	public PolygonColumn(String name, boolean optional, boolean doublePrecision, boolean storeAltitude, boolean storeAccuracy, boolean storeTime, boolean storeProvider, Polygon defaultValue)
	{
		this(name, new LocationColumn("Point", false, doublePrecision, storeAltitude, false, false, storeAccuracy, storeTime, storeProvider), optional, defaultValue);
	}
	
	private PolygonColumn(String name, Column<Location> locationCol, boolean optional, Polygon defaultValue)
	{
		super(name, locationCol, optional, 0, GetMaxLengthForSizeFieldSize(0, SIZE_FIELD_BITS), defaultValue);
	}
	
	@Override
	public PolygonColumn copy()
	{
		return new PolygonColumn(name, singleColumn.copy(), optional, defaultValue);
	}

	@Override
	protected Polygon getNewList(int minimumCapacity)
	{
		return new Polygon(minimumCapacity);
	}
	
	@Override
	protected Polygon getNewList(Collection<Location> points)
	{
		return new Polygon(points);
	}
	
	@Override
	public String getTypeString()
	{
		return Polygon.class.getSimpleName();
	}

	@Override
	public void accept(ColumnVisitor visitor)
	{
		visitor.visit(this);
	}

}
