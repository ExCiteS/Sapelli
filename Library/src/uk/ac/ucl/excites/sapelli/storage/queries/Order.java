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
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;

/**
 * Class to represent the desired ordering of RecordsQuery results 
 * 
 * @author mstevens
 */
public class Order implements Comparator<Record>
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
		return By(new ColumnOrdering(by, DEFAULT_DIRECTION));
	}
	
	static public Order By(ColumnPointer<?> by)
	{
		return By(new ColumnOrdering(by, DEFAULT_DIRECTION));
	}
	
	static public <C extends Column<?>> Order AscendingBy(C by)
	{
		return By(new ColumnOrdering(by, ASCENDING_DIRECTION));
	}
	
	static public Order AscendingBy(ColumnPointer<?> by)
	{
		return By(new ColumnOrdering(by, ASCENDING_DIRECTION));
	}
	
	static public <C extends Column<?>> Order DescendingBy(C by)
	{
		return By(new ColumnOrdering(by, DESCENDING_DIRECTION));
	}
	
	static public Order DescendingBy(ColumnPointer<?> by)
	{
		return By(new ColumnOrdering(by, DESCENDING_DIRECTION));
	}
	
	static public Order By(Column<?>... columns)
	{
		ColumnOrdering[] orderings = columns == null ? null : new ColumnOrdering[columns.length];
		if(columns != null)
			for(int c = 0; c < columns.length; c++)
				orderings[c] = new ColumnOrdering(columns[c], DEFAULT_DIRECTION);
		return By(orderings);
	}
	
	static public Order By(ColumnPointer<?>... columnPointers)
	{
		ColumnOrdering[] orderings = columnPointers == null ? null : new ColumnOrdering[columnPointers.length];
		if(columnPointers != null)
			for(int c = 0; c < columnPointers.length; c++)
				orderings[c] = new ColumnOrdering(columnPointers[c], DEFAULT_DIRECTION);
		return By(orderings);
	}
	
	static public Order By(Ordering... orderings)
	{
		return new Order(orderings == null || orderings.length == 0 ? Collections.<Ordering> emptyList() : Arrays.asList(orderings));
	}
	
	static public final Ordering BY_MODEL_SCHEMA_ASC = new ModelSchemaOrdering(ASCENDING_DIRECTION);
	
	static public final Ordering BY_MODEL_SCHEMA_DESC = new ModelSchemaOrdering(DESCENDING_DIRECTION);

	static public final Ordering BY_MODEL_SCHEMA = BY_MODEL_SCHEMA_ASC;
	
	// DYNAMICS -----------------------------------------------------
	public final List<Ordering> orderings;
	
	private Order(List<Ordering> byColumns)
	{
		this.orderings = new ArrayList<Ordering>(byColumns);
	}
	
	public Order invert()
	{
		List<Ordering> invertedOrderings = new ArrayList<Ordering>(orderings.size());
		for(Ordering ordering : orderings)
			invertedOrderings.add(ordering.invert());
		return new Order(invertedOrderings);
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
	public int compare(Record r1, Record r2)
	{
		for(Ordering ordering : orderings)
		{
			int result = ordering.compare(r1, r2);
			if(result != 0)
				return result;
		}
		return 0;
	}
	
	/**
	 * @param records - should not be null!
	 */
	public void sort(List<Record> records)
	{
		Collections.sort(records, this);
	}
	
	/**
	 * @author mstevens
	 *
	 */
	static public abstract class Ordering implements Comparator<Record>
	{
		
		public final boolean direction;

		/**
		 * @param direction
		 */
		public Ordering(boolean direction)
		{
			this.direction = direction;
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
		
		/**
		 * @return inverted version of this Ordering
		 */
		public abstract Ordering invert();
		
	}
	
	/**
	 * @author mstevens
	 *
	 */
	static public class ColumnOrdering extends Ordering
	{
		
		static public <C extends Column<?>> ColumnOrdering By(C by)
		{
			return new ColumnOrdering(new ColumnPointer<C>(by), DEFAULT_DIRECTION);
		}
		
		static public ColumnOrdering By(ColumnPointer<?> by)
		{
			return new ColumnOrdering(by, DEFAULT_DIRECTION);
		}
		
		static public <C extends Column<?>> ColumnOrdering AscendingBy(C by)
		{
			return new ColumnOrdering(new ColumnPointer<C>(by), ASCENDING_DIRECTION);
		}
		
		static public ColumnOrdering AscendingBy(ColumnPointer<?> by)
		{
			return new ColumnOrdering(by, ASCENDING_DIRECTION);
		}
		
		static public <C extends Column<?>> ColumnOrdering DescendingBy(C by)
		{
			return new ColumnOrdering(new ColumnPointer<C>(by), DESCENDING_DIRECTION);
		}
		
		static public ColumnOrdering DescendingBy(ColumnPointer<?> by)
		{
			return new ColumnOrdering(by, DESCENDING_DIRECTION);
		}
		
		private final ColumnPointer<?> by;

		/**
		 * @param by
		 * @param direction
		 */
		public ColumnOrdering(Column<?> by, boolean direction)
		{
			this(new ColumnPointer<>(by), direction);
		}
		
		/**
		 * @param by
		 * @param direction
		 */
		public ColumnOrdering(ColumnPointer<?> by, boolean direction)
		{
			super(direction);
			this.by = by;
		}

		/**
		 * @return the by
		 */
		public ColumnPointer<?> getBy()
		{
			return by;
		}

		@Override
		public int compare(Record r1, Record r2)
		{
			return (isAsc() ? by.getComparator() : Collections.reverseOrder(by.getComparator())).compare(r1, r2);
		}

		@Override
		public ColumnOrdering invert()
		{
			return new ColumnOrdering(by, !direction);
		}
		
	}
	
	/**
	 * @author mstevens
	 *
	 */
	private static class ModelSchemaOrdering extends Ordering
	{
		
		static private final Comparator<Record> COMPARATOR = new Comparator<Record>()
		{
			@Override
			public int compare(Record r1, Record r2)
			{
				if(r1 == null)
					return r2 == null ? 0 : Integer.MIN_VALUE;
				if(r2 == null)
					return Integer.MAX_VALUE;
				else
				{
					int modelComp = Long.compare(r1.getSchema().model.id, r2.getSchema().model.id);
					if(modelComp != 0)
						return modelComp;
					else
						return Integer.compare(r1.getSchema().modelSchemaNumber, r2.getSchema().modelSchemaNumber); 
				}
			}
		};

		/**
		 * @param direction
		 */
		public ModelSchemaOrdering(boolean direction)
		{
			super(direction);
		}
		
		@Override
		public int compare(Record r1, Record r2)
		{
			return (isAsc() ? COMPARATOR : Collections.reverseOrder(COMPARATOR)).compare(r1, r2);
		}

		@Override
		public Ordering invert()
		{
			return this == BY_MODEL_SCHEMA_ASC ? BY_MODEL_SCHEMA_DESC : BY_MODEL_SCHEMA_ASC;
		}
		
	}
	
}