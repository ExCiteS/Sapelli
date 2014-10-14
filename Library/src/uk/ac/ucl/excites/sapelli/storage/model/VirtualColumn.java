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

package uk.ac.ucl.excites.sapelli.storage.model;

import java.io.IOException;
import java.io.Serializable;
import java.text.ParseException;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * A "virtual", read-only(!) column which maps the values of a "real" column (the sourceColumn, of type ST)
 * to a different type/representation, which can be handled by another "targetColumn" (of type TT).
 * The sourceColumn is assumed to be part of a schema on its own, the target column should never be added to a schema directly.
 * 
 * @param <ST> source type
 * @param <TT> target ("virtual") type
 * 
 * @author mstevens
 */
public class VirtualColumn<TT, ST> extends Column<TT>
{
	
	private static final long serialVersionUID = 2L;
	private static final char NAME_SEPARATOR = '-';

	/**
	 * the "real" column
	 */
	private final Column<ST> sourceColumn;
	
	/**
	 * the "virtual" column
	 */
	private final Column<TT> targetColumn;
	
	private final ValueMapper<TT, ST> valueMapper;

	public VirtualColumn(Column<ST> sourceColumn, Column<TT> targetColumn, ValueMapper<TT, ST> valueMapper)
	{
		super(targetColumn.getType(), sourceColumn.name + NAME_SEPARATOR + targetColumn.name, targetColumn.optional);
		if(sourceColumn == null || targetColumn == null || valueMapper == null)
			throw new NullPointerException("sourceColumn, targetColumn & valueMapper cannot be null!");
		if(sourceColumn == targetColumn)
			throw new IllegalArgumentException("source- & targetColumn cannot be the same!");
		if(sourceColumn.optional != targetColumn.optional)
			throw new IllegalArgumentException("Optionality of target & source column must be the same!");
		this.sourceColumn = sourceColumn;
		this.targetColumn = targetColumn;
		this.valueMapper = valueMapper;
	}
	
	@Override
	public VirtualColumn<TT, ST> copy()
	{
		return new VirtualColumn<TT, ST>(sourceColumn, targetColumn.copy(), valueMapper);
	}
	
	@Override
	public void storeValue(Record record, TT value) throws IllegalArgumentException, NullPointerException, UnsupportedOperationException
	{
		throw new UnsupportedOperationException("VirtualColumns are read-only!");
	}
	
	/**
	 * Overridden to retrieve value from sourceColum and convert it.
	 *  
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#retrieveValue(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	public TT retrieveValue(Record record)
	{
		// Retrieve value from sourceColumn:
		ST sourceValue = sourceColumn.retrieveValue(record);
		if(sourceValue == null)
			return null;
		// Return converted value:
		return valueMapper.mapValue(sourceValue);
	}

	@Override
	public TT parse(String value) throws ParseException, IllegalArgumentException, NullPointerException
	{
		return targetColumn.parse(value);
	}

	@Override
	public String toString(TT value)
	{
		return targetColumn.toString(value);
	}

	@Override
	protected void write(TT value, BitOutputStream bitStream) throws IOException
	{
		targetColumn.write(value, bitStream);
	}

	@Override
	protected TT read(BitInputStream bitStream) throws IOException
	{
		return targetColumn.read(bitStream);
	}

	@Override
	protected void validate(TT value) throws IllegalArgumentException
	{
		targetColumn.validate(value);
	}

	@Override
	protected TT copy(TT value)
	{
		return targetColumn.copy(value);
	}

	@Override
	protected int _getMaximumSize()
	{
		return targetColumn._getMaximumSize();
	}

	@Override
	protected int _getMinimumSize()
	{
		return targetColumn._getMinimumSize();
	}

	@Override
	protected boolean equalRestrictions(Column<TT> otherColumn)
	{
		if(this.getClass().isInstance(otherColumn))
		{
			VirtualColumn<?, ?> otherVirtualColumn = (VirtualColumn<?, ?>) otherColumn;
			return 	this.targetColumn.equals(otherVirtualColumn.targetColumn) &&
					this.valueMapper.equals(otherVirtualColumn.valueMapper);
			// Note: do not include sourceColumn here otherwise we create an endless loop!
		}
		return false;
	}
	
	@Override
    public int hashCode()
	{
		int hash = super.hashCode();
		hash = 31 * hash + targetColumn.hashCode();
		hash = 31 * hash + valueMapper.hashCode();
		return hash;
		// Note: do not include sourceColumn here otherwise we create an endless loop!
	}

	@Override
	public void accept(ColumnVisitor visitor)
	{
		if(visitor.includeVirtualColumns())
			visitor.visit(this);
	}
	
	public Column<ST> getSourceColumn()
	{
		return sourceColumn;
	}
	
	public Column<TT> getTargetColumn()
	{
		return targetColumn;
	}
	
	/**
	 * Mapping of source to target types
	 *
	 * @param <ST> source type
	 * @param <TT> target ("virtual") type
	 * 
	 * @author mstevens
	 */
	static public abstract class ValueMapper<TT, ST> implements Serializable
	{
	
		private static final long serialVersionUID = 2L;

		/**
		 * Converts values from source type ST to target type TT
		 * 
		 * @param nonNullValue
		 * @return
		 */
		public abstract TT mapValue(ST nonNullValue);
		
		public abstract int hashCode();
		
		@Override
		public boolean equals(Object obj)
		{
			if(this == obj)
				return true;
			if(this.getClass().isInstance(obj))
				return this.hashCode() == ((ValueMapper<?, ?>) obj).hashCode();
			return false;
		}
		
	}

}
