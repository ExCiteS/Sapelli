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
 * @author mstevens, benelliott
 */
public abstract class ListColumn<L extends List<T>, T> extends Column<L> implements ListLikeColumn<L>
{
	
	// STATICS ----------------------------------------------------------------
	static private final long serialVersionUID = 2L;
	
	static public final int DEFAULT_MINIMUM_LENGTH = 0; // list items
	static public final int DEFAULT_MAXIMUM_LENGTH = Integer.MAX_VALUE; // like any List or other Collection (Collection#size() returns an int)
	static public final char DEFAULT_SERIALISATION_DELIMITER = '|';
	static public final char DEFAULT_SERIALISATION_SEPARATOR = ';';
	static public final char DEFAULT_VALUE_SERIALISATION_DELIMITER = ValueSet.DEFAULT_SERIALISATION_DELIMITER;
	
	static protected int GetMaxLengthForSizeFieldSize(int minLength, int sizeBits)
	{
		return IntegerRangeMapping.ForSize(minLength, sizeBits).highBound().intValue();
	}
	
	// DYNAMICS ---------------------------------------------------------------
	private final IntegerRangeMapping sizeField;
	protected final Column<T> singleColumn;
	protected final char serialisationDelimiter;
	protected final char serialisationSeparator;

	/**
	 * Creates a {@link ListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and maximum length of {@value #DEFAULT_MAXIMUM_LENGTH}.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 */
	public ListColumn(String name, Column<T> singleColumn, boolean optional)
	{
		this(name, singleColumn, optional, null);
	}
	
	/**
	 * Creates a {@link ListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and maximum length of {@value #DEFAULT_MAXIMUM_LENGTH}.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param defaultValue
	 */
	public ListColumn(String name, Column<T> singleColumn, boolean optional, L defaultValue)
	{
		this(name, singleColumn, optional, defaultValue, DEFAULT_SERIALISATION_DELIMITER, DEFAULT_SERIALISATION_SEPARATOR);
	}
	
	/**
	 * Creates a {@link ListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and maximum length of {@value #DEFAULT_MAXIMUM_LENGTH}.
	 * 
	 * The {@code serialisationSeparator} char is used to separate individual list values in String serialisation.
	 * The {@code serialisationDelimiter} is used to wrap the entire serialised List value.
	 * Occurrences of the {@code serialisationSeparator} char inside String representations of individual list
	 * elements will be escaped by wrapping the value in the serialisation delimiter of the {@code singleColumn}
	 * (if {@link Column#isApplyingSerialisationDelimiting()} returns {@code true}) or in the {@link #DEFAULT_VALUE_SERIALISATION_DELIMITER}.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param serialisationDelimiter
	 * @param serialisationSeparator
	 * 
	 * @see #ListColumn(String, Column, boolean, int, char, char, char, Character, Character)
	 */
	public ListColumn(String name, Column<T> singleColumn, boolean optional, char serialisationDelimiter, char serialisationSeparator)
	{
		this(name, singleColumn, optional, null, serialisationDelimiter, serialisationSeparator);
	}
	
	/**
	 * Creates a {@link ListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and maximum length of {@value #DEFAULT_MAXIMUM_LENGTH}.
	 * 
	 * The {@code serialisationSeparator} char is used to separate individual list values in String serialisation.
	 * The {@code serialisationDelimiter} is used to wrap the entire serialised List value.
	 * Occurrences of the {@code serialisationSeparator} char inside String representations of individual list
	 * elements will be escaped by wrapping the value in the serialisation delimiter of the {@code singleColumn}
	 * (if {@link Column#isApplyingSerialisationDelimiting()} returns {@code true}) or in the {@link #DEFAULT_VALUE_SERIALISATION_DELIMITER}.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param defaultValue
	 * @param serialisationDelimiter
	 * @param serialisationSeparator
	 * 
	 * @see #ListColumn(String, Column, boolean, int, char, char, char, Character, Character)
	 */
	public ListColumn(String name, Column<T> singleColumn, boolean optional, L defaultValue, char serialisationDelimiter, char serialisationSeparator)
	{
		this(name, singleColumn, optional, DEFAULT_MAXIMUM_LENGTH, serialisationDelimiter, serialisationSeparator);
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
		this(name, singleColumn, optional, maxLength, null);
	}
	
	/**
	 * Creates a {@link ListColumn.Simple} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and the given maximum length.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param maxLength
	 * @param defaultValue
	 */
	public ListColumn(String name, Column<T> singleColumn, boolean optional, int maxLength, L defaultValue)
	{
		this(name, singleColumn, optional, maxLength, defaultValue, DEFAULT_SERIALISATION_DELIMITER, DEFAULT_SERIALISATION_SEPARATOR);
	}
	
	/**
	 * Creates a {@link ListColumn.Simple} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and the given maximum length.
	 * 
	 * The {@code serialisationSeparator} char is used to separate individual list values in String serialisation.
	 * The {@code serialisationDelimiter} is used to wrap the entire serialised List value.
	 * Occurrences of the {@code serialisationSeparator} char inside String representations of individual list
	 * elements will be escaped by wrapping the value in the serialisation delimiter of the {@code singleColumn}
	 * (if {@link Column#isApplyingSerialisationDelimiting()} returns {@code true}) or in the {@link #DEFAULT_VALUE_SERIALISATION_DELIMITER}.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param maxLength
	 * @param serialisationDelimiter
	 * @param serialisationSeparator
	 * 
	 * @see #ListColumn(String, Column, boolean, int, char, char, char, Character, Character)
	 */
	public ListColumn(String name, Column<T> singleColumn, boolean optional, int maxLength, char serialisationDelimiter, char serialisationSeparator)
	{
		this(name, singleColumn, optional, maxLength, null, serialisationDelimiter, serialisationSeparator);
	}
	
	/**
	 * Creates a {@link ListColumn.Simple} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and the given maximum length.
	 * 
	 * The {@code serialisationSeparator} char is used to separate individual list values in String serialisation.
	 * The {@code serialisationDelimiter} is used to wrap the entire serialised List value.
	 * Occurrences of the {@code serialisationSeparator} char inside String representations of individual list
	 * elements will be escaped by wrapping the value in the serialisation delimiter of the {@code singleColumn}
	 * (if {@link Column#isApplyingSerialisationDelimiting()} returns {@code true}) or in the {@link #DEFAULT_VALUE_SERIALISATION_DELIMITER}.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param maxLength
	 * @param defaultValue
	 * @param serialisationDelimiter
	 * @param serialisationSeparator
	 * 
	 * @see #ListColumn(String, Column, boolean, int, char, char, char, Character, Character)
	 */
	public ListColumn(String name, Column<T> singleColumn, boolean optional, int maxLength, L defaultValue, char serialisationDelimiter, char serialisationSeparator)
	{
		this(name, singleColumn, optional, DEFAULT_MINIMUM_LENGTH, maxLength, defaultValue, serialisationDelimiter, serialisationSeparator);
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
		this(name, singleColumn, optional, minLength, maxLength, null);
	}
	
	/**
	 * Creates a {@link ListColumn} with the given minimum and maximum lengths.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param minLength
	 * @param maxLength
	 * @param defaultValue
	 */
	public ListColumn(String name, Column<T> singleColumn, boolean optional, int minLength, int maxLength, L defaultValue)
	{
		this(name, singleColumn, optional, minLength, maxLength, defaultValue, DEFAULT_SERIALISATION_DELIMITER, DEFAULT_SERIALISATION_SEPARATOR);
	}

	/**
	 * Creates a {@link ListColumn} with the given minimum and maximum lengths.
	 * 
	 * The {@code serialisationSeparator} char is used to separate individual list values in String serialisation.
	 * The {@code serialisationDelimiter} is used to wrap the entire serialised List value.
	 * Occurrences of the {@code serialisationSeparator} char inside String representations of individual list
	 * elements will be escaped by wrapping the value in the serialisation delimiter of the {@code singleColumn}
	 * (if {@link Column#isApplyingSerialisationDelimiting()} returns {@code true}) or in the {@link #DEFAULT_VALUE_SERIALISATION_DELIMITER}.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param minLength
	 * @param maxLength
	 * @param serialisationDelimiter
	 * @param serialisationSeparator
	 */
	public ListColumn(String name, Column<T> singleColumn, boolean optional, int minLength, int maxLength, char serialisationDelimiter, char serialisationSeparator)
	{
		this(name, singleColumn, optional, minLength, maxLength, null, serialisationDelimiter, serialisationSeparator);
	}
	
	/**
	 * Creates a {@link ListColumn} with the given minimum and maximum lengths.
	 * 
	 * The {@code serialisationSeparator} char is used to separate individual list values in String serialisation.
	 * The {@code serialisationDelimiter} is used to wrap the entire serialised List value.
	 * Occurrences of the {@code serialisationSeparator} char inside String representations of individual list
	 * elements will be escaped by wrapping the value in the serialisation delimiter of the {@code singleColumn}
	 * (if {@link Column#isApplyingSerialisationDelimiting()} returns {@code true}) or in the {@link #DEFAULT_VALUE_SERIALISATION_DELIMITER}.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param minLength
	 * @param maxLength
	 * @param defaultValue
	 * @param serialisationDelimiter
	 * @param serialisationSeparator
	 */
	public ListColumn(String name, Column<T> singleColumn, boolean optional, int minLength, int maxLength, L defaultValue, char serialisationDelimiter, char serialisationSeparator)
	{
		super(name, optional, defaultValue);
		if(singleColumn instanceof ListColumn)
			throw new IllegalArgumentException("Cannot nest ListColumns!");
		if(singleColumn instanceof VirtualColumn)
			throw new IllegalArgumentException("SingleColumn of ListColumn cannot be virtual!");
		this.singleColumn = singleColumn;
		this.sizeField = new IntegerRangeMapping(minLength, maxLength, true); // allow "empty" (0-sized) in case the list must always have exactly one element (no fewer, no more)
		this.serialisationDelimiter = serialisationDelimiter;
		this.serialisationSeparator = serialisationSeparator;
	}

	/**
	 * Creates a new empty List of type {@code <L>}.
	 * 
	 * @return
	 */
	public abstract L getNewList();
	
	/**
	 * Creates a new empty List of type {@code <L>} and the given {@code minimumCapacity}.
	 * 
	 * @param minimumCapacity
	 * @return
	 */
	public final L getNewList(int minimumCapacity)
	{
		if(minimumCapacity < 0 || minimumCapacity > getMaximumLength())
			throw new IllegalArgumentException("Invalid minimumCapacity (max: " + getMaximumLength() + ")!");
		return _getNewList(minimumCapacity);
	}
	
	/**
	 * Creates a new empty List of type {@code <L>} and the given {@code minimumCapacity}.
	 * 
	 * @param minimumCapacity - can be assumed to be <= {@link #getMaximumLength()}
	 * @return
	 */
	protected abstract L _getNewList(int minimumCapacity);
	
	/**
	 * Creates a new List of type {@code <L>} initialised with the given {@code elements}.
	 * 
	 * @param elements
	 * @return
	 */
	protected final L getNewList(Collection<T> elements)
	{
		if(elements == null)
			return getNewList();
		else if(elements.size() > getMaximumLength())
			throw new IllegalArgumentException("Too many elements given (max: " + getMaximumLength() + ")!");
		else
			return _getNewList(elements);
	}
	
	/**
	 * Creates a new List of type {@code <L>} initialised with the given {@code elements}.
	 * 
	 * @param elements - can be assumed to be non-{@code null} and with size <= {@link #getMaximumLength()}
	 * @return
	 */
	protected abstract L _getNewList(Collection<T> elements);
	
	/**
	 * @return the singleColumn
	 */
	public Column<T> getSingleColumn()
	{
		return singleColumn;
	}

	/**
	 * @return the serialisationDelimiter
	 */
	@Override
	public Character getSerialisationDelimiter()
	{
		return serialisationDelimiter;
	}

	/**
	 * @return the serialisationSeparator
	 */
	public char getSerialisationSeparator()
	{
		return serialisationSeparator;
	}
	
	/**
	 * The given String is be expected to be wrapped/escaped using {@link #serialisationDelimiter}s.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#parse(java.lang.String)
	 */
	@Override
	public L parse(String listString) throws ParseException, IllegalArgumentException, NullPointerException
	{
		return parse(listString, false); // delimited!
	}

	/**
	 * If {@code undelimited} is {@code false} the given {@code valueString} is expected to be wrapped/escaped using {@link #serialisationDelimiter}s.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.ListLikeColumn#parse(java.lang.String, boolean)
	 */
	@Override
	public L parse(String listString, boolean undelimited) throws ParseException, IllegalArgumentException, NullPointerException
	{
		if(!undelimited)
		{
			// Perform delimiter checks:
			if(listString.length() < 2)
				throw new ParseException("String is not delimited by " + serialisationDelimiter, 0);
			if(listString.charAt(0) != serialisationDelimiter)
				throw new ParseException("String does not begin with " + serialisationDelimiter, 0);
			if(listString.charAt(listString.length() - 1) != serialisationDelimiter)
				throw new ParseException("String does not end with " + serialisationDelimiter, listString.length() - 1);
			// Remove serialisation delimiters:
			listString =  listString.substring(1, listString.length() - 1);
		}
		
		// Parse:
		List<T> parsedValues = new ArrayList<>(StringUtils.countOccurances(listString, serialisationSeparator));
		final char valueDelimiter = singleColumn.getSerialisationDelimiter() != null ? singleColumn.getSerialisationDelimiter() : DEFAULT_VALUE_SERIALISATION_DELIMITER;
		int valueDelimiterCount = 0;
		StringBuilder valueStringBldr = new StringBuilder();
		for(char c : listString.toCharArray())
		{
			if(c == valueDelimiter)
			{	// count the number of valueDelimiters we've passed
				valueDelimiterCount++;
				// Append char:
				valueStringBldr.append(c); // !!!
			}
			else if(c == serialisationSeparator && valueDelimiterCount % 2 == 0)
			{	// if delimiterCount is even this means we are not *inside* a value and this is an actual serialisation separator
				String valueString = valueStringBldr.toString();
				
				// If the column does not apply it's own serialisation delimiting then
				//	it could be that valueString is wrapped using the default value serialisation delimiters:
				if(!singleColumn.isApplyingSerialisationDelimiting())
					valueString = StringUtils.deescapeByDoublingAndWrapping(valueString, DEFAULT_VALUE_SERIALISATION_DELIMITER);
				
				// Parse value:
				parsedValues.add(singleColumn.stringToValue(valueString)); // if the column applies it's own delimiters these will be removed/deescaped; validation will be performed
				
				// We are done with this column:
				valueDelimiterCount = 0; // !!!
				valueStringBldr.setLength(0); // reset valueString builder!
			}
			else
			{	
				// Append char:
				valueStringBldr.append(c); // !!!
			}
		}
		
		// Check if we didn't get too many values:
		if(parsedValues.size() > getMaximumLength())
			throw new IllegalArgumentException("Found more values than allowed (got: " + parsedValues.size() + "; max: " + getMaximumLength() + ")!");
		
		// Make list:
		L values = getNewList(parsedValues);
		
		// Done!:
		return values;
	}

	/**
	 * The returned String will be wrapped/escaped using {@link #serialisationDelimiter}s in order to preserve the difference between a null list and an empty list.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#toString(java.lang.Object)
	 */
	@Override
	public String toString(L values)
	{
		return toString(values, false); // delimited!
	}
	
	/**
	 * If {@code undelimited} is {@code false} the returned String will be wrapped/escaped using {@link #serialisationDelimiter}s in order to preserve the difference between a null list and an empty list.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.ListLikeColumn#toString(java.lang.Object, boolean)
	 * @see ValueSet#serialise(boolean, java.util.Set)
	 */
	@Override
	public String toString(L values, boolean undelimited)
	{
		StringBuilder bldr = new StringBuilder();
		for(T value : values)
		{
			String valueString = singleColumn.valueToString(value); // will return empty String for null values
			
			// 	If the column does not apply it's own serialisation delimiting then
			//	 we may have to wrap the valueString in the default value serialisation delimiters:
			if(!singleColumn.isApplyingSerialisationDelimiting())
				valueString = StringUtils.escapeByDoublingAndWrapping(valueString, new char[] { serialisationSeparator },  DEFAULT_VALUE_SERIALISATION_DELIMITER, /*don't force:*/ false);
			
			bldr.append(valueString == null ? "" /*just in case*/ : valueString);
			bldr.append(serialisationSeparator); // there are as many separators as elements (this allows us to preserve the difference between an empty list and a list with 1 null element)
		}
		 // unless undelimited, wrap in serialisationDelimiters (this allows us to preserve the difference between a null list and an empty list):
		return undelimited ? bldr.toString() : StringUtils.escapeByDoublingAndWrapping(bldr.toString(), null, serialisationDelimiter, /*force:*/ true);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#write(java.lang.Object, uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream, boolean)
	 */
	@Override
	protected void write(L values, BitOutputStream bitStream, boolean lossless) throws IOException
	{
		// Write size:
		sizeField.write(values.size(), bitStream);
		// Write values:
		for(T value : values)
			singleColumn.writeValue(value, bitStream, lossless);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#read(uk.ac.ucl.excites.sapelli.shared.io.BitInputStream, boolean)
	 */
	@Override
	protected L read(BitInputStream bitStream, boolean lossless) throws IOException
	{
		// Read size:
		int size = sizeField.readInt(bitStream);
		// Read values:
		L values = getNewList(size);
		for(int i = 0; i < size; i++)
			values.add(singleColumn.readValue(bitStream, lossless));
		return values;
	}
	
	@Override
	protected void validate(L values) throws IllegalArgumentException
	{
		if(values.size() < getMinimumLength())
			throw new IllegalArgumentException(getTypeString() + " does not contain enough " + singleColumn.getTypeString() + "s, minimum is " + getMinimumLength() + ", given value has " + values.size() + ".");
		if(values.size() > getMaximumLength())
			throw new IllegalArgumentException(getTypeString() + " contains too many " + singleColumn.getTypeString() + "s, maximum is " + getMaximumLength() + ", given value has " + values.size() + ".");
		int v = 0;
		for(T value : values)
		{
			if(!singleColumn.isValidValue(value))
			{
				String valueString = value.toString();
				try
				{
					valueString = singleColumn.valueToString(value);
				}
				catch(Exception ignore) {} 
				throw new IllegalArgumentException("Element " + v + " (" + valueString + ") is invalid!");
			}
			v++;
		}
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
	protected int getMaximumValueSize(boolean lossless)
	{
		return sizeField.size() + (getMaximumLength() * singleColumn.getMaximumSize(lossless));
	}

	@Override
	protected int getMinimumValueSize(boolean lossless)
	{
		return sizeField.size() + (getMinimumLength() * singleColumn.getMinimumSize(lossless));
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#canBeLossy()
	 */
	@Override
	public boolean canBeLossy()
	{
		return singleColumn.canBeLossy();
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
				ListColumn<L, T> that = (ListColumn<L, T>) otherColumn;
				return	this.sizeField.equals(that.sizeField) &&
						this.serialisationDelimiter == that.serialisationDelimiter &&
						this.serialisationSeparator == that.serialisationSeparator &&
						this.singleColumn.equals(that.singleColumn); 
			}
			catch(ClassCastException cce)
			{
				cce.printStackTrace(System.err);
				return false;
			}
		}
		return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode();
		hash = 31 * hash + sizeField.hashCode();
		hash = 31 * hash + serialisationDelimiter;
		hash = 31 * hash + serialisationSeparator;
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
		 * 
		 * @see #ListColumn(String, Column, boolean)
		 */
		public Simple(String name, Column<T> singleColumn, boolean optional)
		{
			super(name, singleColumn, optional);
		}
		
		/**
		 * Creates a {@link ListColumn.Simple} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and maximum length of {@value #DEFAULT_MAXIMUM_LENGTH}.
		 * 
		 * @param singleColumn
		 * @param optional
		 * @param defaultValue
		 * 
		 * @see #ListColumn(String, Column, boolean, List)
		 */
		public Simple(String name, Column<T> singleColumn, boolean optional, List<T> defaultValue)
		{
			super(name, singleColumn, optional, defaultValue);
		}
		
		/**
		 * Creates a {@link ListColumn.Simple} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and maximum length of {@value #DEFAULT_MAXIMUM_LENGTH}.
		 * 
		 * @param name
		 * @param singleColumn
		 * @param optional
		 * @param serialisationDelimiter
		 * @param serialisationSeparator
		 * @param separatorEscape
		 * @param separatorEscapePrefix
		 * 
		 * @see #ListColumn(String, Column, boolean, int, char, char)
		 */
		public Simple(String name, Column<T> singleColumn, boolean optional, char serialisationDelimiter, char serialisationSeparator)
		{
			super(name, singleColumn, optional, serialisationDelimiter, serialisationSeparator);
		}
		
		/**
		 * Creates a {@link ListColumn.Simple} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and maximum length of {@value #DEFAULT_MAXIMUM_LENGTH}.
		 * 
		 * @param name
		 * @param singleColumn
		 * @param optional
		 * @param defaultValue
		 * @param serialisationDelimiter
		 * @param serialisationSeparator
		 * 
		 * @see #ListColumn(String, Column, boolean, int, List, char, char)
		 */
		public Simple(String name, Column<T> singleColumn, boolean optional, List<T> defaultValue, char serialisationDelimiter, char serialisationSeparator)
		{
			super(name, singleColumn, optional, defaultValue, serialisationDelimiter, serialisationSeparator);
		}
		
		/**
		 * Creates a {@link ListColumn.Simple} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and the given maximum length.
		 * 
		 * @param name
		 * @param singleColumn
		 * @param optional
		 * @param maxLength
		 * 
		 * @see #ListColumn(String, Column, boolean, int)
		 */
		public Simple(String name, Column<T> singleColumn, boolean optional, int maxLength)
		{
			super(name, singleColumn, optional, maxLength);
		}
		
		/**
		 * Creates a {@link ListColumn.Simple} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and the given maximum length.
		 * 
		 * @param name
		 * @param singleColumn
		 * @param optional
		 * @param maxLength
		 * @param defaultValue
		 * 
		 * @see #ListColumn(String, Column, boolean, int, List)
		 */
		public Simple(String name, Column<T> singleColumn, boolean optional, int maxLength, List<T> defaultValue)
		{
			super(name, singleColumn, optional, maxLength, defaultValue);
		}
		
		/**
		 * Creates a {@link ListColumn.Simple} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and the given maximum length.
		 * 
		 * @param name
		 * @param singleColumn
		 * @param optional
		 * @param maxLength
		 * @param serialisationDelimiter
		 * @param serialisationSeparator
		 * 
		 * @see #ListColumn(String, Column, boolean, int, char, char)
		 */
		public Simple(String name, Column<T> singleColumn, boolean optional, int maxLength, char serialisationDelimiter, char serialisationSeparator)
		{
			super(name, singleColumn, optional, maxLength, serialisationDelimiter, serialisationSeparator);
		}
		
		/**
		 * Creates a {@link ListColumn.Simple} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and the given maximum length.
		 * 
		 * @param name
		 * @param singleColumn
		 * @param optional
		 * @param maxLength
		 * @param defaultValue
		 * @param serialisationDelimiter
		 * @param serialisationSeparator
		 * 
		 * @see #ListColumn(String, Column, boolean, int, List, char, char)
		 */
		public Simple(String name, Column<T> singleColumn, boolean optional, int maxLength, List<T> defaultValue, char serialisationDelimiter, char serialisationSeparator)
		{
			super(name, singleColumn, optional, maxLength, defaultValue, serialisationDelimiter, serialisationSeparator);
		}
		
		/**
		 * Creates a {@link ListColumn.Simple} with the given minimum and maximum lengths.
		 * 
		 * @param name
		 * @param singleColumn
		 * @param optional
		 * @param minLength
		 * @param maxLength
		 * 
		 * @see #ListColumn(String, Column, boolean, int, int)
		 */
		public Simple(String name, Column<T> singleColumn, boolean optional, int minLength, int maxLength)
		{
			super(name, singleColumn, optional, minLength, maxLength);
		}
		
		/**
		 * Creates a {@link ListColumn.Simple} with the given minimum and maximum lengths.
		 * 
		 * @param name
		 * @param singleColumn
		 * @param optional
		 * @param minLength
		 * @param maxLength
		 * @param defaultValue
		 * 
		 * @see #ListColumn(String, Column, boolean, int, int, List)
		 */
		public Simple(String name, Column<T> singleColumn, boolean optional, int minLength, int maxLength, List<T> defaultValue)
		{
			super(name, singleColumn, optional, minLength, maxLength, defaultValue);
		}
		
		/**
		 * Creates a {@link ListColumn.Simple} with the given minimum and maximum lengths.
		 * 
		 * @param name
		 * @param singleColumn
		 * @param optional
		 * @param minLength
		 * @param maxLength
		 * @param serialisationDelimiter
		 * @param serialisationSeparator
		 * 
		 * @see #ListColumn(String, Column, boolean, int, char, char)
		 */
		public Simple(String name, Column<T> singleColumn, boolean optional, int minLength, int maxLength, char serialisationDelimiter, char serialisationSeparator)
		{
			super(name, singleColumn, optional, minLength, maxLength, serialisationDelimiter, serialisationSeparator);
		}

		/**
		 * Creates a {@link ListColumn.Simple} with the given minimum and maximum lengths.
		 * 
		 * @param name
		 * @param singleColumn
		 * @param optional
		 * @param minLength
		 * @param maxLength
		 * @param defaultValue
		 * @param serialisationDelimiter
		 * @param serialisationSeparator
		 * 
		 * @see #ListColumn(String, Column, boolean, int, List, char, char)
		 */
		public Simple(String name, Column<T> singleColumn, boolean optional, int minLength, int maxLength, List<T> defaultValue, char serialisationDelimiter, char serialisationSeparator)
		{
			super(name, singleColumn, optional, minLength, maxLength, defaultValue, serialisationDelimiter, serialisationSeparator);
		}

		@Override
		public List<T> getNewList()
		{
			return new ArrayList<T>();
		}
		
		@Override
		protected List<T> _getNewList(int minimumCapacity)
		{
			return new ArrayList<T>(minimumCapacity);
		}
		
		@Override
		protected List<T> _getNewList(Collection<T> elements)
		{
			return new ArrayList<T>(elements);
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
		public List<T> convert(Object value)
		{
			return (List<T>) (value == null ? null : (value instanceof List ? value : new ArrayList<T>((Collection<? extends T>) value)));
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
