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

package uk.ac.ucl.excites.sapelli.storage.queries;

import java.util.Collections;
import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.ComparableColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;

/**
 * Class to represent the desired ordering of RecordsQuery results 
 * 
 * TODO multicol ordering, ordering by schema
 * 
 * @author mstevens
 */
public class Order
{
	
	// STATICS ------------------------------------------------------
	static public final boolean ASCENDING_DIRECTION = true;
	static public final boolean DESCENDING_DIRECTION = !ASCENDING_DIRECTION;
	static public final boolean DEFAULT_DIRECTION = ASCENDING_DIRECTION; // as in SQL
	static public final Order UNDEFINED = new Order(null, DEFAULT_DIRECTION);

	static public Order By(ComparableColumn<?> by)
	{
		return new Order(new ColumnPointer(by), DEFAULT_DIRECTION);
	}
	
	static public Order By(ColumnPointer by)
	{
		return new Order(by, DEFAULT_DIRECTION);
	}
	
	static public Order AscendingBy(ComparableColumn<?> by)
	{
		return new Order(new ColumnPointer(by), ASCENDING_DIRECTION);
	}
	
	static public Order AscendingBy(ColumnPointer by)
	{
		return new Order(by, ASCENDING_DIRECTION);
	}
	
	static public Order DescendingBy(ComparableColumn<?> by)
	{
		return new Order(new ColumnPointer(by), DESCENDING_DIRECTION);
	}
	
	static public Order DescendingBy(ColumnPointer by)
	{
		return new Order(by, DESCENDING_DIRECTION);
	}
	
	// DYNAMICS -----------------------------------------------------
	public final ColumnPointer by;
	public final boolean direction; // true: ASC, false: DESC
	
	/**
	 * @param by
	 * @param direction
	 */
	private Order(ColumnPointer by, boolean direction)
	{
		if(by != null && !(by.getColumn() instanceof ComparableColumn))
			throw new IllegalArgumentException("ColumnPointer does not point to a ComparatorColumn");
		this.by = by;
		this.direction = direction;
	}
	
	public Order invert()
	{
		return new Order(by, !direction);
	}
	
	/**
	 * @return
	 */
	public boolean isDefined()
	{
		return by != null;
	}
	
	/**
	 * @return
	 */
	public boolean isUndefined()
	{
		return by == null;
	}

	/**
	 * @return the column to order by
	 */
	public ColumnPointer getBy()
	{
		return by;
	}

	/**
	 * @return whether or not the order is ASCending
	 */
	public boolean isAsc()
	{
		return direction;
	}
	
	/**
	 * @return whether or not the order is DESCending
	 */
	public boolean isDesc()
	{
		return !direction;
	}
	
	public void sort(List<Record> records)
	{
		if(isDefined())
			Collections.sort(records, isAsc() ? by.getComparator() : Collections.reverseOrder(by.getComparator()));
	}
	
}