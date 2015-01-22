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
	static public final char DEFAULT_SERIALISATION_DELIMITER_OPEN = '(';
	static public final char DEFAULT_SERIALISATION_DELIMITER_CLOSE = ')';
	static public final char DEFAULT_SERIALISATION_SEPARATOR = ';';
	static public final char DEFAULT_SERIALISATION_SEPARATOR_ESCAPE = ':';
	static public final char DEFAULT_SERIALISATION_SEPARATOR_ESCAPE_PREFIX = '/';
	
	static protected int GetMaxLengthForSizeFieldSize(int minLength, int sizeBits)
	{
		return IntegerRangeMapping.ForSize(minLength, sizeBits).highBound().intValue();
	}
	
	// DYNAMICS ---------------------------------------------------------------
	private final IntegerRangeMapping sizeField;
	protected final Column<T> singleColumn;
	protected final char serialisationDelimiterOpen;
	protected final char serialisationDelimiterClose;
	protected final char separator;
	protected final Character separatorEscape;
	protected final Character separatorEscapePrefix;

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
		this(name, singleColumn, optional, defaultValue, DEFAULT_SERIALISATION_DELIMITER_OPEN, DEFAULT_SERIALISATION_DELIMITER_CLOSE, DEFAULT_SERIALISATION_SEPARATOR, DEFAULT_SERIALISATION_SEPARATOR_ESCAPE, DEFAULT_SERIALISATION_SEPARATOR_ESCAPE_PREFIX);
	}
	
	/**
	 * Creates a {@link ListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and maximum length of {@value #DEFAULT_MAXIMUM_LENGTH}.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param serialisationDelimiterOpen
	 * @param serialisationDelimiterClose
	 * @param separator
	 * @param separatorEscape
	 * @param separatorEscapePrefix
	 * 
	 * @see #ListColumn(String, Column, boolean, int, char, char, char, Character, Character)
	 */
	public ListColumn(String name, Column<T> singleColumn, boolean optional, char serialisationDelimiterOpen, char serialisationDelimiterClose, char separator, Character separatorEscape, Character separatorEscapePrefix)
	{
		this(name, singleColumn, optional, null, serialisationDelimiterOpen, serialisationDelimiterClose, separator, separatorEscape, separatorEscapePrefix);
	}
	
	/**
	 * Creates a {@link ListColumn} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and maximum length of {@value #DEFAULT_MAXIMUM_LENGTH}.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param defaultValue
	 * @param serialisationDelimiterOpen
	 * @param serialisationDelimiterClose
	 * @param separator
	 * @param separatorEscape
	 * @param separatorEscapePrefix
	 * 
	 * @see #ListColumn(String, Column, boolean, int, char, char, char, Character, Character)
	 */
	public ListColumn(String name, Column<T> singleColumn, boolean optional, L defaultValue, char serialisationDelimiterOpen, char serialisationDelimiterClose, char separator, Character separatorEscape, Character separatorEscapePrefix)
	{
		this(name, singleColumn, optional, DEFAULT_MAXIMUM_LENGTH, serialisationDelimiterOpen, serialisationDelimiterClose, separator, separatorEscape, separatorEscapePrefix);
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
		this(name, singleColumn, optional, maxLength, defaultValue, DEFAULT_SERIALISATION_DELIMITER_OPEN, DEFAULT_SERIALISATION_DELIMITER_CLOSE, DEFAULT_SERIALISATION_SEPARATOR, DEFAULT_SERIALISATION_SEPARATOR_ESCAPE, DEFAULT_SERIALISATION_SEPARATOR_ESCAPE_PREFIX);
	}
	
	/**
	 * Creates a {@link ListColumn.Simple} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and the given maximum length.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param maxLength
	 * @param serialisationDelimiterOpen
	 * @param serialisationDelimiterClose
	 * @param separator
	 * @param separatorEscape
	 * @param separatorEscapePrefix
	 * 
	 * @see #ListColumn(String, Column, boolean, int, char, char, char, Character, Character)
	 */
	public ListColumn(String name, Column<T> singleColumn, boolean optional, int maxLength, char serialisationDelimiterOpen, char serialisationDelimiterClose, char separator, Character separatorEscape, Character separatorEscapePrefix)
	{
		this(name, singleColumn, optional, maxLength, null, serialisationDelimiterOpen, serialisationDelimiterClose, separator, separatorEscape, separatorEscapePrefix);
	}
	
	/**
	 * Creates a {@link ListColumn.Simple} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and the given maximum length.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param maxLength
	 * @param defaultValue
	 * @param serialisationDelimiterOpen
	 * @param serialisationDelimiterClose
	 * @param separator
	 * @param separatorEscape
	 * @param separatorEscapePrefix
	 * 
	 * @see #ListColumn(String, Column, boolean, int, char, char, char, Character, Character)
	 */
	public ListColumn(String name, Column<T> singleColumn, boolean optional, int maxLength, L defaultValue, char serialisationDelimiterOpen, char serialisationDelimiterClose, char separator, Character separatorEscape, Character separatorEscapePrefix)
	{
		this(name, singleColumn, optional, DEFAULT_MINIMUM_LENGTH, maxLength, defaultValue, serialisationDelimiterOpen, serialisationDelimiterClose, separator, separatorEscape, separatorEscapePrefix);
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
		this(name, singleColumn, optional, minLength, maxLength, defaultValue, DEFAULT_SERIALISATION_DELIMITER_OPEN, DEFAULT_SERIALISATION_DELIMITER_CLOSE, DEFAULT_SERIALISATION_SEPARATOR, DEFAULT_SERIALISATION_SEPARATOR_ESCAPE, DEFAULT_SERIALISATION_SEPARATOR_ESCAPE_PREFIX);
	}

	/**
	 * Creates a {@link ListColumn} with the given minimum and maximum lengths.
	 * 
	 * The {@code separator} char is used to separate individual list values in String serialisation.
	 * If neither {@code separatorEscape} and {@code separatorEscapePrefix} are {@code null} occurrences
	 * of the {@code separator} char inside String representations of individual list elements will be
	 * escaped during serialisation of the list: occurrences of the {@code separator} char will be replaced
	 * by {@code separatorEscapePrefix}+{@code separatorEscape}, and occurrences of {@code separatorEscapePrefix}
	 * will be replaced by {@code separatorEscapePrefix}+{@code separatorEscapePrefix}. In this case the
	 * {@code separator}, {@code separatorEscape} and {@code separatorEscapePrefix} chars must be all different.
	 * When {@code separatorEscape} and {@code separatorEscapePrefix} are both {@code null} no escaping
	 * of {@code separator} chars occurring inside String representations of individual list elements is
	 * possible, and values (i.e. lists) containing elements whose String representation do contain the
	 * {@code separator} char will be rejected.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param minLength
	 * @param maxLength
	 * @param serialisationDelimiterOpen
	 * @param serialisationDelimiterClose
	 * @param separator
	 * @param separatorEscape
	 * @param separatorEscapePrefix
	 */
	public ListColumn(String name, Column<T> singleColumn, boolean optional, int minLength, int maxLength, char serialisationDelimiterOpen, char serialisationDelimiterClose, char separator, Character separatorEscape, Character separatorEscapePrefix)
	{
		this(name, singleColumn, optional, minLength, maxLength, null, serialisationDelimiterOpen, serialisationDelimiterClose, separator, separatorEscape, separatorEscapePrefix);
	}
	
	/**
	 * Creates a {@link ListColumn} with the given minimum and maximum lengths.
	 * 
	 * The {@code separator} char is used to separate individual list values in String serialisation.
	 * If neither {@code separatorEscape} and {@code separatorEscapePrefix} are {@code null} occurrences
	 * of the {@code separator} char inside String representations of individual list elements will be
	 * escaped during serialisation of the list: occurrences of the {@code separator} char will be replaced
	 * by {@code separatorEscapePrefix}+{@code separatorEscape}, and occurrences of {@code separatorEscapePrefix}
	 * will be replaced by {@code separatorEscapePrefix}+{@code separatorEscapePrefix}. In this case the
	 * {@code separator}, {@code separatorEscape} and {@code separatorEscapePrefix} chars must be all different.
	 * When {@code separatorEscape} and {@code separatorEscapePrefix} are both {@code null} no escaping
	 * of {@code separator} chars occurring inside String representations of individual list elements is
	 * possible, and values (i.e. lists) containing elements whose String representation do contain the
	 * {@code separator} char will be rejected.
	 * 
	 * @param name
	 * @param singleColumn
	 * @param optional
	 * @param minLength
	 * @param maxLength
	 * @param defaultValue
	 * @param serialisationDelimiterOpen
	 * @param serialisationDelimiterClose
	 * @param separator
	 * @param separatorEscape
	 * @param separatorEscapePrefix
	 */
	public ListColumn(String name, Column<T> singleColumn, boolean optional, int minLength, int maxLength, L defaultValue, char serialisationDelimiterOpen, char serialisationDelimiterClose, char separator, Character separatorEscape, Character separatorEscapePrefix)
	{
		super(name, optional, defaultValue);
		if(singleColumn instanceof ListColumn)
			throw new IllegalArgumentException("Cannot nest " + getClass().getSimpleName() + "s!");
		this.singleColumn = singleColumn;
		this.sizeField = new IntegerRangeMapping(minLength, maxLength, true); // allow "empty" (0-sized) in case the list must always have exactly one element (no fewer, no more)
		this.serialisationDelimiterOpen = serialisationDelimiterOpen;
		this.serialisationDelimiterClose = serialisationDelimiterClose;
		this.separator = separator;
		this.separatorEscape = separatorEscape;
		this.separatorEscapePrefix = separatorEscapePrefix;
		if(isSeparatorEscapingEnabled() && (separator == separatorEscape || separator == separatorEscapePrefix || separatorEscape == separatorEscapePrefix))
			throw new IllegalArgumentException("The code separator, separatorEscape and separatorEscapePrefix chars must be all different.");
	}
	
	protected abstract L getNewList(int minimumCapacity);
	
	protected abstract L getNewList(Collection<T> elements);
	
	/**
	 * @return the singleColumn
	 */
	public Column<T> getSingleColumn()
	{
		return singleColumn;
	}

	/**
	 * @return the serialisationDelimiterOpen
	 */
	public char getSerialisationDelimiterOpen()
	{
		return serialisationDelimiterOpen;
	}

	/**
	 * @return the serialisationDelimiterClose
	 */
	public char getSerialisationDelimiterClose()
	{
		return serialisationDelimiterClose;
	}

	/**
	 * @return the separator
	 */
	public char getSeparator()
	{
		return separator;
	}

	/**
	 * @return the separatorEscape
	 */
	public Character getSeparatorEscape()
	{
		return separatorEscape;
	}

	/**
	 * @return the separatorEscapePrefix
	 */
	public Character getSeparatorEscapePrefix()
	{
		return separatorEscapePrefix;
	}

	public boolean isSeparatorEscapingEnabled()
	{
		return separatorEscape != null && separatorEscapePrefix != null;
	}
	
	/**
	 * @param str
	 * @return
	 * @throws IllegalArgumentException if the String needs escaping (i.e. the separator char occurs in it) but escaping is disabled
	 */
	protected String escapeSeparator(String str) throws IllegalArgumentException
	{
		if(str != null && str.indexOf(separator) > -1) // check if escaping is needed for the given String
		{
			if(isSeparatorEscapingEnabled())
				return StringUtils.escape(str, separator, separatorEscape, separatorEscapePrefix);
			else
				throw new IllegalArgumentException("Given string (" + str + ") needs escaping but escaping is disabled!");
		}
		return str;
	}
	
	protected String deescapeSeparator(String str)
	{
		if(str != null && isSeparatorEscapingEnabled())
			return StringUtils.deescape(str, separator, separatorEscape, separatorEscapePrefix);
		return str;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#parse(java.lang.String)
	 */
	@Override
	public L parse(String listString) throws ParseException, IllegalArgumentException, NullPointerException
	{
		return parse(listString, false); // delimited!
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.ListLikeColumn#parse(java.lang.String, boolean)
	 */
	@Override
	public L parse(String listString, boolean undelimited) throws ParseException, IllegalArgumentException, NullPointerException
	{
		if(!undelimited)
		{
			// Perform delimiter checks:
			if(listString.length() < 2)
				throw new ParseException("String is not delimited by " + serialisationDelimiterOpen + " and " + serialisationDelimiterClose, 0);
			if(listString.charAt(0) != serialisationDelimiterOpen)
				throw new ParseException("String does not begin with " + serialisationDelimiterOpen, 0);
			if(listString.charAt(listString.length() - 1) != serialisationDelimiterClose)
				throw new ParseException("String does not end with " + serialisationDelimiterClose, listString.length() - 1);
			// Remove serialisation delimiters:
			listString =  listString.substring(1, listString.length() - 1);
		}
		// Parse:
		int size = StringUtils.countOccurances(listString, separator);
		L values = getNewList(size);
		int startPointer = 0;
		int sepPointer = 0;
		while((sepPointer = listString.indexOf(separator, startPointer)) > -1)
		{
			values.add(singleColumn.stringToValue(deescapeSeparator(listString.substring(startPointer, sepPointer))));
			startPointer = sepPointer + 1;
		}
		return values;
	}

	/**
	 * The returned String is delimited by serialisation delimiters in order to preserve the difference between a null list and an empty list.
	 * If separator escaping is enabled ({@link #isSeparatorEscapingEnabled()}) separators occurring within the Strings representing individual list elements will escaped.
	 * Warning: no escaping of serialisation delimiters takes place at this level! So occurrences of serialisation delimiters within the Strings representing individual list elements will be left unchanged. 
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#toString(java.lang.Object)
	 */
	@Override
	public String toString(L values)
	{
		return toString(values, false); // delimited!
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.ListLikeColumn#toString(java.lang.Object, boolean)
	 */
	@Override
	public String toString(L values, boolean undelimited)
	{
		StringBuilder bldr = new StringBuilder();
		for(T value : values)
		{
			String valueString = escapeSeparator(singleColumn.valueToString(value));
			bldr.append(valueString != null ? valueString : ""); // null values are represented by empty String
			bldr.append(separator); // there are as many separators as elements (this allows us to preserve the difference between an empty list and a list with 1 null element)
		}
		return (undelimited ? "" : serialisationDelimiterOpen) + bldr.toString() + (undelimited ? "" : serialisationDelimiterClose); // unless undelimited: wrap in serialisation open/close chars (this allows us to preserve the difference between a null list and an empty list)
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
		int v = 0;
		for(T value : values)
		{
			boolean valid = singleColumn.isValidValue(value);
			if(valid)
				try
				{
					escapeSeparator(singleColumn.valueToString(value));
				}
				catch(Exception e)
				{
					valid = false;
				}
			if(!valid)
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
				ListColumn<L, T> that = (ListColumn<L, T>) otherColumn;
				return	this.sizeField.equals(that.sizeField) &&
						this.serialisationDelimiterOpen == that.serialisationDelimiterOpen &&
						this.serialisationDelimiterClose == that.serialisationDelimiterClose &&
						this.separator == that.separator &&
						this.separatorEscape == that.separatorEscape &&
						this.separatorEscapePrefix == that.separatorEscapePrefix &&
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
		hash = 31 * hash + serialisationDelimiterOpen;
		hash = 31 * hash + serialisationDelimiterClose;
		hash = 31 * hash + separator;
		hash = 31 * hash + (separatorEscape != null ? separatorEscape.hashCode() : 0);
		hash = 31 * hash + (separatorEscapePrefix != null ? separatorEscapePrefix.hashCode() : 0);
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
		 * @param serialisationDelimiterOpen
		 * @param serialisationDelimiterClose
		 * @param separator
		 * @param separatorEscape
		 * @param separatorEscapePrefix
		 * 
		 * @see #ListColumn(String, Column, boolean, int, char, char, char, Character, Character)
		 */
		public Simple(String name, Column<T> singleColumn, boolean optional, char serialisationDelimiterOpen, char serialisationDelimiterClose, char separator, Character separatorEscape, Character separatorEscapePrefix)
		{
			super(name, singleColumn, optional, serialisationDelimiterOpen, serialisationDelimiterClose, separator, separatorEscape, separatorEscapePrefix);
		}
		
		/**
		 * Creates a {@link ListColumn.Simple} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and maximum length of {@value #DEFAULT_MAXIMUM_LENGTH}.
		 * 
		 * @param name
		 * @param singleColumn
		 * @param optional
		 * @param defaultValue
		 * @param serialisationDelimiterOpen
		 * @param serialisationDelimiterClose
		 * @param separator
		 * @param separatorEscape
		 * @param separatorEscapePrefix
		 * 
		 * @see #ListColumn(String, Column, boolean, int, List, char, char, char, Character, Character)
		 */
		public Simple(String name, Column<T> singleColumn, boolean optional, List<T> defaultValue, char serialisationDelimiterOpen, char serialisationDelimiterClose, char separator, Character separatorEscape, Character separatorEscapePrefix)
		{
			super(name, singleColumn, optional, defaultValue, serialisationDelimiterOpen, serialisationDelimiterClose, separator, separatorEscape, separatorEscapePrefix);
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
		 * @param serialisationDelimiterOpen
		 * @param serialisationDelimiterClose
		 * @param separator
		 * @param separatorEscape
		 * @param separatorEscapePrefix
		 * 
		 * @see #ListColumn(String, Column, boolean, int, char, char, char, Character, Character)
		 */
		public Simple(String name, Column<T> singleColumn, boolean optional, int maxLength, char serialisationDelimiterOpen, char serialisationDelimiterClose, char separator, Character separatorEscape, Character separatorEscapePrefix)
		{
			super(name, singleColumn, optional, maxLength, serialisationDelimiterOpen, serialisationDelimiterClose, separator, separatorEscape, separatorEscapePrefix);
		}
		
		/**
		 * Creates a {@link ListColumn.Simple} with minimum length of {@value #DEFAULT_MINIMUM_LENGTH} and the given maximum length.
		 * 
		 * @param name
		 * @param singleColumn
		 * @param optional
		 * @param maxLength
		 * @param defaultValue
		 * @param serialisationDelimiterOpen
		 * @param serialisationDelimiterClose
		 * @param separator
		 * @param separatorEscape
		 * @param separatorEscapePrefix
		 * 
		 * @see #ListColumn(String, Column, boolean, int, List, char, char, char, Character, Character)
		 */
		public Simple(String name, Column<T> singleColumn, boolean optional, int maxLength, List<T> defaultValue, char serialisationDelimiterOpen, char serialisationDelimiterClose, char separator, Character separatorEscape, Character separatorEscapePrefix)
		{
			super(name, singleColumn, optional, maxLength, defaultValue, serialisationDelimiterOpen, serialisationDelimiterClose, separator, separatorEscape, separatorEscapePrefix);
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
		 * @param serialisationDelimiterOpen
		 * @param serialisationDelimiterClose
		 * @param separator
		 * @param separatorEscape
		 * @param separatorEscapePrefix
		 * 
		 * @see #ListColumn(String, Column, boolean, int, char, char, char, Character, Character)
		 */
		public Simple(String name, Column<T> singleColumn, boolean optional, int minLength, int maxLength, char serialisationDelimiterOpen, char serialisationDelimiterClose, char separator, Character separatorEscape, Character separatorEscapePrefix)
		{
			super(name, singleColumn, optional, minLength, maxLength, serialisationDelimiterOpen, serialisationDelimiterClose, separator, separatorEscape, separatorEscapePrefix);
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
		 * @param serialisationDelimiterOpen
		 * @param serialisationDelimiterClose
		 * @param separator
		 * @param separatorEscape
		 * @param separatorEscapePrefix
		 * 
		 * @see #ListColumn(String, Column, boolean, int, List, char, char, char, Character, Character)
		 */
		public Simple(String name, Column<T> singleColumn, boolean optional, int minLength, int maxLength, List<T> defaultValue, char serialisationDelimiterOpen, char serialisationDelimiterClose, char separator, Character separatorEscape, Character separatorEscapePrefix)
		{
			super(name, singleColumn, optional, minLength, maxLength, defaultValue, serialisationDelimiterOpen, serialisationDelimiterClose, separator, separatorEscape, separatorEscapePrefix);
		}

		@Override
		protected List<T> getNewList(int minimumCapacity)
		{
			return new ArrayList<T>(minimumCapacity);
		}
		
		@Override
		protected List<T> getNewList(Collection<T> elements)
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
