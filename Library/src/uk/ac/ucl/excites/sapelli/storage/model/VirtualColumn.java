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

	/**
	 * @param sourceColumn
	 * @param targetColumn
	 * @param valueMapper
	 */
	public VirtualColumn(Column<ST> sourceColumn, Column<TT> targetColumn, ValueMapper<TT, ST> valueMapper)
	{
		this(sourceColumn.name + NAME_SEPARATOR + targetColumn.name, sourceColumn, targetColumn, valueMapper);
	}
	
	/**
	 * @param name
	 * @param sourceColumn
	 * @param targetColumn
	 * @param valueMapper
	 */
	public VirtualColumn(String name, Column<ST> sourceColumn, Column<TT> targetColumn, ValueMapper<TT, ST> valueMapper)
	{
		super(name, targetColumn.optional, sourceColumn.defaultValue == null ? null : valueMapper.mapValue(sourceColumn.defaultValue, null));
		if(sourceColumn == null || targetColumn == null || valueMapper == null)
			throw new NullPointerException("sourceColumn, targetColumn & valueMapper cannot be null!");
		if(sourceColumn instanceof VirtualColumn || targetColumn instanceof VirtualColumn)
			throw new IllegalArgumentException("source- & targetColumn cannot be VirtualColumns themselves!");
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
	public TT storeValue(ValueSet<?> valueSet, TT value) throws IllegalArgumentException, NullPointerException, UnsupportedOperationException
	{
		throw new UnsupportedOperationException("VirtualColumns are read-only!");
	}
	
	/**
	 * Overridden to retrieve value from sourceColum and convert it.
	 *  
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#retrieveValue(uk.ac.ucl.excites.sapelli.storage.model.ValueSet)
	 */
	@Override
	public <VS extends ValueSet<CS>, CS extends ColumnSet> TT retrieveValue(VS valueSet)
	{
		// Retrieve value from sourceColumn:
		ST sourceValue = sourceColumn.retrieveValue(valueSet);
		if(sourceValue == null)
			return null;
		// Return converted value:
		return valueMapper.mapValue(sourceValue, new UnmodifiableValueSet<CS>(valueSet));
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

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#getSerialisationDelimiter()
	 */
	@Override
	public Character getSerialisationDelimiter()
	{
		return targetColumn.getSerialisationDelimiter();
	}

	@Override
	protected void write(TT value, BitOutputStream bitStream, boolean lossless) throws IOException
	{
		targetColumn.write(value, bitStream, lossless);
	}

	@Override
	protected TT read(BitInputStream bitStream, boolean lossless) throws IOException
	{
		return targetColumn.read(bitStream, lossless);
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
	protected int getMaximumValueSize(boolean lossless)
	{
		return targetColumn.getMaximumValueSize(lossless);
	}

	@Override
	protected int getMinimumValueSize(boolean lossless)
	{
		return targetColumn.getMinimumValueSize(lossless);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#canBeLossy()
	 */
	@Override
	public boolean canBeLossy()
	{
		return targetColumn.canBeLossy();
	}

	@Override
	protected boolean equalRestrictions(Column<TT> otherColumn)
	{
		if(this.getClass().isInstance(otherColumn))
		{
			VirtualColumn<?, ?> that = (VirtualColumn<?, ?>) otherColumn;
			return 	this.targetColumn.equals(that.targetColumn) &&
					this.valueMapper.equals(that.valueMapper);
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
		 * @param nonNullSourceValue the source value of type <ST>
		 * @param valueSet an unmodifiable version of the valueSet in which the sourceValue occurs (may be null)
		 * @return
		 */
		public abstract TT mapValue(ST nonNullSourceValue, UnmodifiableValueSet<?> valueSet);
		
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

	@Override
	public Class<TT> getType()
	{
		return targetColumn.getType();
	}

}
