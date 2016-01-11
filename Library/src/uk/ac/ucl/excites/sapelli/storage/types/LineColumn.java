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
 * A column for {@link Line}s, implemented as a {@link ListColumn} subclass.
 * 
 * @author mstevens
 */
public class LineColumn extends ListColumn<Line, Location>
{
	
	static private final long serialVersionUID = 2L;
	
	static public final int SIZE_FIELD_BITS = 16;

	public LineColumn(String name, boolean optional, boolean doublePrecision, boolean storeAltitude, boolean storeAccuracy, boolean storeTime, boolean storeProvider)
	{
		this(name, optional, doublePrecision, storeAltitude, storeAccuracy, storeTime, storeProvider, null);
	}
	
	public LineColumn(String name, boolean optional, boolean doublePrecision, boolean storeAltitude, boolean storeAccuracy, boolean storeTime, boolean storeProvider, Line defaultValue)
	{
		this(name, new LocationColumn("Point", false, doublePrecision, storeAltitude, false, false, storeAccuracy, storeTime, storeProvider), optional, defaultValue);
	}
	
	private LineColumn(String name, Column<Location> locationCol, boolean optional, Line defaultValue)
	{
		super(name, locationCol, optional, 0, GetMaxLengthForSizeFieldSize(0, SIZE_FIELD_BITS), defaultValue);
	}
	
	@Override
	public LineColumn copy()
	{
		return new LineColumn(name, singleColumn.copy(), optional, defaultValue);
	}

	@Override
	public Line getNewList()
	{
		return new Line();
	}
	
	@Override
	protected Line _getNewList(int minimumCapacity)
	{
		return new Line(minimumCapacity);
	}
	
	@Override
	protected Line _getNewList(Collection<Location> points)
	{
		return new Line(points);
	}
	
	@Override
	public String getTypeString()
	{
		return Line.class.getSimpleName();
	}

	@Override
	public void accept(ColumnVisitor visitor)
	{
		visitor.visit(this);
	}

}
