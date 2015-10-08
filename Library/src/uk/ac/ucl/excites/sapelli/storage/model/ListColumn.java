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
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.shared.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * A Column which takes {@link List}s a values.
 *
 * @param <L> the list type (extends List<T>)
 * @param <T> the content type
 * 
 * @author mstevens
 */
public abstract class ListColumn<L extends List<T>, T> extends Column<L>
{
	
	// STATICS ----------------------------------------------------------------
	static private final long serialVersionUID = 2L;
	
	static public final int DEFAULT_MINIMUM_LENGTH = 0; // list items
	static public final int DEFAULT_MAXIMUM_LENGTH = Integer.MAX_VALUE; // like any List or other Collection (Collection#size() returns an int)
	static public final char SERIALISATION_SEPARATOR = ';';
	static public final char SERIALISATION_SEPARATOR_ESCAPE = ':';
	static public final char SERIALISATION_SEPARATOR_ESCAPE_PREFIX = '/';
	
	static protected int GetMaxLengthForSizeFieldSize(int minLength, int sizeBits)
	{
		return IntegerRangeMapping.ForSize(minLength, sizeBits).highBound().intValue();
	}
	
	// DYNAMICS ---------------------------------------------------------------
	private final IntegerRangeMapping sizeField;
	protected final Column<T> singleColumn;

	/**
	 * Creates a {@link ListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and maximum length of {@value #DEFAULT_MAXIMUM_LENGTH}.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 */
	public ListColumn(String name, Column<T> singleColumn, boolean optional)
	{
		this(name, singleColumn, optional, DEFAULT_MINIMUM_LENGTH, DEFAULT_MAXIMUM_LENGTH);
	}

	/**
	 * Creates a {@link ListColumn.Simple} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and the given maximum length.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param maxLength
	 */
	public ListColumn(String name, Column<T> singleColumn, boolean optional, int maxLength)
	{
		this(name, singleColumn, optional, DEFAULT_MINIMUM_LENGTH, maxLength);
	}

	/**
	 * Creates a {@link ListColumn} with the given minimum and maximum lengths.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param minLength
	 * @param maxLength
	 */
	public ListColumn(String name, Column<T> singleColumn, boolean optional, int minLength, int maxLength)
	{
		super(name, optional);
		this.singleColumn = singleColumn;
		this.sizeField = new IntegerRangeMapping(minLength, maxLength);
	}
	
	protected abstract L getNewList(int minimumCapacity);
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#parse(java.lang.String)
	 */
	@Override
	public L parse(String listStr) throws ParseException, IllegalArgumentException, NullPointerException
	{
		String[] parts = listStr.split("\\" + SERIALISATION_SEPARATOR);
		L values = getNewList(parts.length);
		for(String v : parts)
			values.add(singleColumn.parse(StringUtils.deescape(v, SERIALISATION_SEPARATOR, SERIALISATION_SEPARATOR_ESCAPE, SERIALISATION_SEPARATOR_ESCAPE_PREFIX)));
		return values;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#toString(java.lang.Object)
	 */
	@Override
	public String toString(L values)
	{
		StringBuilder bldr = new StringBuilder();
		boolean first = true;
		for(T value : values)
		{
			// Separator:
			if(first)
				first = false;
			else
				bldr.append(SERIALISATION_SEPARATOR);
			// Value:
			if(value != null)
				bldr.append(StringUtils.escape(singleColumn.toString(value), SERIALISATION_SEPARATOR, SERIALISATION_SEPARATOR_ESCAPE, SERIALISATION_SEPARATOR_ESCAPE_PREFIX));
		}
		return bldr.toString();
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#write(java.lang.Object, uk.ac.ucl.excites.sapelli.storage.io.BitOutputStream)
	 */
	@Override
	protected void write(L values, BitOutputStream bitStream) throws IOException
	{
		// Write size:
		sizeField.write(values.size(), bitStream);
		// Write values:
		for(T value : values)
			singleColumn.writeValue(value, bitStream);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#read(uk.ac.ucl.excites.sapelli.storage.io.BitInputStream)
	 */
	@Override
	protected L read(BitInputStream bitStream) throws IOException
	{
		// Read size:
		int size = sizeField.readInt(bitStream);
		// Read values:
		L values = getNewList(size);
		for(int i = 0; i < size; i++)
			values.add(singleColumn.readValue(bitStream));
		return values;
	}
	
	@Override
	protected void validate(L values) throws IllegalArgumentException
	{
		if(values.size() < getMinimumLength())
			throw new IllegalArgumentException(getTypeString() + " does not contain enough " + singleColumn.getTypeString() + "s, minimum is " + getMinimumLength() + ", given value has " + values.size() + ".");
		if(values.size() > getMaximumLength())
			throw new IllegalArgumentException(getTypeString() + " contains too many " + singleColumn.getTypeString() + "s, maximum is " + getMaximumLength() + ", given value has " + values.size() + ".");
	}

	@Override
	protected L copy(L values)
	{
		L copy = getNewList(values.size());
		for(T value : values)
			copy.add(singleColumn.copy(value));
		return copy;
	}

	@Override
	protected int _getMaximumSize()
	{
		return sizeField.size() + (getMaximumLength() * singleColumn.getMaximumSize());
	}

	@Override
	protected int _getMinimumSize()
	{
		return sizeField.size() + (getMinimumLength() * singleColumn.getMinimumSize());
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#equalRestrictions(uk.ac.ucl.excites.sapelli.storage.model.Column)
	 */
	@Override
	protected boolean equalRestrictions(Column<L> otherColumn)
	{
		if(otherColumn instanceof ListColumn)
		{
			try
			{
				@SuppressWarnings("unchecked")
				ListColumn<L, T> other = (ListColumn<L, T>) otherColumn;
				return this.singleColumn.equals(other.singleColumn) && this.sizeField.equals(other.sizeField);
			}
			catch(ClassCastException cce)
			{
				cce.printStackTrace(System.err);
				return false;
			}
		}
		return false;
	}

	/**
	 * @return the maxLength
	 */
	public int getMinimumLength()
	{
		return sizeField.lowBound().intValue();
	}
	
	/**
	 * @return the maxLength
	 */
	public int getMaximumLength()
	{
		return sizeField.highBound().intValue();
	}

	@Override
	public String getTypeString()
	{
		return singleColumn.getTypeString() + List.class.getSimpleName();
	}
	
	@Override
    public int hashCode()
	{
		int hash = super.hashCode();
		hash = 31 * hash + sizeField.hashCode();
		hash = 31 * hash + singleColumn.hashCode();
		return hash;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Class<L> getType()
	{
		return (Class<L>) (Class<?>) List.class;
	}
	
	/**
	 * "Simple" ListColumn implementation which is only generic in terms of the content type (<T>) and not in the list type (<L>).
	 * 
	 * @param <T> the content type
	 *
	 * @author mstevens
	 */
	public static class Simple<T> extends ListColumn<List<T>, T>
	{
		
		static private final long serialVersionUID = 2L;

		/**
		 * Creates a {@link ListColumn.Simple} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and maximum length of {@value #DEFAULT_MAXIMUM_LENGTH}.
		 * 
		 * @param name
		 * @param singleColumn
		 * @param optional
		 */
		public Simple(String name, Column<T> singleColumn, boolean optional)
		{
			super(name, singleColumn, optional);
		}
		
		/**
		 * Creates a {@link ListColumn.Simple} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and the given maximum length.
		 * 
		 * @param name
		 * @param singleColumn
		 * @param optional
		 * @param maxLength
		 */
		public Simple(String name, Column<T> singleColumn, boolean optional, int maxLength)
		{
			super(name, singleColumn, optional, maxLength);
		}
		
		/**
		 * Creates a {@link ListColumn.Simple} with the given minimum and maximum lengths.
		 * 
		 * @param name
		 * @param singleColumn
		 * @param optional
		 * @param minLength
		 * @param maxLength
		 */
		public Simple(String name, Column<T> singleColumn, boolean optional, int minLength, int maxLength)
		{
			super(name, singleColumn, optional, minLength, maxLength);
		}

		@Override
		protected List<T> getNewList(int minimumCapacity)
		{
			return new ArrayList<T>(minimumCapacity);
		}
		
		/**
		 * Converts Collection<T>s to (Array)List<T>s
		 * 
		 * @param value possibly null
		 * @return
		 * @throws ClassCastException when the value is not a {@link java.util.Collection}
		 * 
		 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#convert(java.lang.Object)
		 */
		@SuppressWarnings("unchecked")
		@Override
		public Object convert(Object value)
		{
			return value == null ? null : (value instanceof List ? value : new ArrayList<T>((Collection<? extends T>) value));
		}
		
		@Override
		public Simple<T> copy()
		{
			return new Simple<T>(name, singleColumn.copy(), optional, getMinimumLength(), getMaximumLength());
		}

		@Override
		public void accept(ColumnVisitor visitor)
		{
			visitor.visit(this);
		}
		
	}
	
}
