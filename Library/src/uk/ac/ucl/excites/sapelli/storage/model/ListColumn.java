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
import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.storage.util.IntegerRangeMapping;

/**
 * @author mstevens
 *
 */
public abstract class ListColumn<L extends List<T>, T> extends Column<L>
{
	
	static private final long serialVersionUID = 2L;
	
	static public final int DEFAULT_MINIMUM_LENGTH = 0; // list items
	static public final int DEFAULT_MAXIMUM_LENGTH = Integer.MAX_VALUE; // like any List or other Collection (Collection#size() returns an int)
	static public final char SERIALISATION_SEPARATOR = ';';
	static public final char SERIALISATION_SEPARATOR_ESCAPE = ':';
	static public final char SERIALISATION_SEPARATOR_ESCAPE_PREFIX = '/';
	
	static protected int GetMaxLengthForSizeFieldSize(int minLength, int sizeBits)
	{
		return (int) IntegerRangeMapping.ForSize(minLength, sizeBits).highBound();
	}
	
	private final IntegerRangeMapping sizeField;
	protected final Column<T> singleColumn;

	public ListColumn(String name, Column<T> singleColumn, boolean optional)
	{
		this(name, singleColumn, optional, DEFAULT_MINIMUM_LENGTH, DEFAULT_MAXIMUM_LENGTH);
	}
	
	public ListColumn(String name, Column<T> singleColumn, boolean optional, int maxLength)
	{
		this(name, singleColumn, optional, DEFAULT_MINIMUM_LENGTH, maxLength);
	}
	
	@SuppressWarnings("unchecked")
	public ListColumn(String name, Column<T> singleColumn, boolean optional, int minLength, int maxLength)
	{
		super((Class<L>) (Class<?>) List.class, name, optional);
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
		int size = (int) sizeField.read(bitStream);
		// Read values:
		L values = getNewList(size);
		for(int i = 0; i < size; i++)
			values.add(singleColumn.readValue(bitStream));
		return values;
	}
	
	@Override
	protected void validate(L values) throws IllegalArgumentException
	{
		if(values.size() < sizeField.lowBound())
			throw new IllegalArgumentException(getTypeString() + " does not contain enough " + singleColumn.getTypeString() + "s, minimum is " + sizeField.lowBound() + ", given value has " + values.size() + ".");
		if(values.size() > sizeField.highBound())
			throw new IllegalArgumentException(getTypeString() + " contains too many " + singleColumn.getTypeString() + "s, maximum is " + sizeField.lowBound() + ", given value has " + values.size() + ".");
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
		return (int) sizeField.lowBound();
	}
	
	/**
	 * @return the maxLength
	 */
	public int getMaximumLength()
	{
		return (int) sizeField.highBound();
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

}
