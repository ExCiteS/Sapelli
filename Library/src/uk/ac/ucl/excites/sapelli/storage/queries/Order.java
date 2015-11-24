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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSet;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;

/**
 * Class to represent the desired ordering of RecordsQuery results 
 * 
 * TODO ordering by schema
 * 
 * @author mstevens
 */
public class Order implements Comparator<ValueSet<?>>
{
	
	// STATICS ------------------------------------------------------
	static public final boolean ASCENDING_DIRECTION = true;
	
	static public final boolean DESCENDING_DIRECTION = !ASCENDING_DIRECTION;
	
	/**
	 * Default order direction is ascending (as in SQL).
	 */
	static public final boolean DEFAULT_DIRECTION = ASCENDING_DIRECTION;
	
	static public final Order UNDEFINED = new Order(Collections.<Ordering> emptyList());

	static public <C extends Column<?>> Order By(C by)
	{
		return By(new Ordering(by, DEFAULT_DIRECTION));
	}
	
	static public Order By(ColumnPointer<?> by)
	{
		return By(new Ordering(by, DEFAULT_DIRECTION));
	}
	
	static public <C extends Column<?>> Order AscendingBy(C by)
	{
		return By(new Ordering(by, ASCENDING_DIRECTION));
	}
	
	static public Order AscendingBy(ColumnPointer<?> by)
	{
		return By(new Ordering(by, ASCENDING_DIRECTION));
	}
	
	static public <C extends Column<?>> Order DescendingBy(C by)
	{
		return By(new Ordering(by, DESCENDING_DIRECTION));
	}
	
	static public Order DescendingBy(ColumnPointer<?> by)
	{
		return By(new Ordering(by, DESCENDING_DIRECTION));
	}
	
	static public Order By(Column<?>... columns)
	{
		Ordering[] orderings = columns == null ? null : new Ordering[columns.length];
		if(columns != null)
			for(int c = 0; c < columns.length; c++)
				orderings[c] = new Ordering(columns[c], DEFAULT_DIRECTION);
		return By(orderings);
	}
	
	static public Order By(ColumnPointer<?>... columnPointers)
	{
		Ordering[] orderings = columnPointers == null ? null : new Ordering[columnPointers.length];
		if(columnPointers != null)
			for(int c = 0; c < columnPointers.length; c++)
				orderings[c] = new Ordering(columnPointers[c], DEFAULT_DIRECTION);
		return By(orderings);
	}
	
	static public Order By(Ordering... orderings)
	{
		return new Order(orderings == null || orderings.length == 0 ? Collections.<Ordering> emptyList() : Arrays.asList(orderings));
	}
	
	// DYNAMICS -----------------------------------------------------
	public final List<Ordering> orderings;
	
	private Order(List<Ordering> byColumns)
	{
		this.orderings = new ArrayList<Ordering>(byColumns);
	}
	
	public Order invert()
	{
		List<Ordering> byReversedColumns = new ArrayList<Ordering>(orderings.size());
		for(Ordering by : orderings)
			byReversedColumns.add(new Ordering(by.by, !by.direction));
		return new Order(byReversedColumns);
	}
	
	/**
	 * @return
	 */
	public boolean isDefined()
	{
		return !isUndefined();
	}
	
	/**
	 * @return
	 */
	public boolean isUndefined()
	{
		return orderings.isEmpty();
	}

	/**
	 * @return the column to order by
	 */
	public List<Ordering> getOrderings()
	{
		return Collections.unmodifiableList(orderings);
	}

	/**
	 * @see http://stackoverflow.com/a/1421458/1084488
	 * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
	 */
	@Override
	public int compare(ValueSet<?> vs1, ValueSet<?> vs2)
	{
		for(Ordering ordering : orderings)
		{
			int result = ordering.compare(vs1, vs2);
			if(result != 0)
				return result;
		}
		return 0;
	}
	
	/**
	 * @param records
	 */
	public <VS extends ValueSet<?>> void sort(List<VS> valueSets)
	{
		Collections.sort(valueSets, this);
	}
	
	/**
	 * @author mstevens
	 *
	 */
	static public class Ordering implements Comparator<ValueSet<?>>
	{
		
		static public <C extends Column<?>> Ordering By(C by)
		{
			return new Ordering(new ColumnPointer<C>(by), DEFAULT_DIRECTION);
		}
		
		static public Ordering By(ColumnPointer<?> by)
		{
			return new Ordering(by, DEFAULT_DIRECTION);
		}
		
		static public <C extends Column<?>> Ordering AscendingBy(C by)
		{
			return new Ordering(new ColumnPointer<C>(by), ASCENDING_DIRECTION);
		}
		
		static public Ordering AscendingBy(ColumnPointer<?> by)
		{
			return new Ordering(by, ASCENDING_DIRECTION);
		}
		
		static public <C extends Column<?>> Ordering DescendingBy(C by)
		{
			return new Ordering(new ColumnPointer<C>(by), DESCENDING_DIRECTION);
		}
		
		static public Ordering DescendingBy(ColumnPointer<?> by)
		{
			return new Ordering(by, DESCENDING_DIRECTION);
		}
		
		private final ColumnPointer<?> by;
		
		public final boolean direction;

		/**
		 * @param by
		 * @param direction
		 */
		public Ordering(Column<?> by, boolean direction)
		{
			this(new ColumnPointer<>(by), direction);
		}
		
		/**
		 * @param by
		 * @param direction
		 */
		public Ordering(ColumnPointer<?> by, boolean direction)
		{
			this.by = by;
			this.direction = direction;
		}

		/**
		 * @return the by
		 */
		public ColumnPointer<?> getBy()
		{
			return by;
		}
		
		/**
		 * @return whether or not the order is ASCending
		 */
		public boolean isAsc()
		{
			return direction == ASCENDING_DIRECTION;
		}
		
		/**
		 * @return whether or not the order is DESCending
		 */
		public boolean isDesc()
		{
			return direction == DESCENDING_DIRECTION;
		}

		@Override
		public int compare(ValueSet<?> vs1, ValueSet<?> vs2)
		{
			return (isAsc() ? by.getComparator() : Collections.reverseOrder(by.getComparator())).compare(vs1, vs2);
		}
		
	}
	
}