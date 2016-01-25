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

package uk.ac.ucl.excites.sapelli.storage.model.columns;
 
import java.io.IOException;
import java.nio.charset.Charset;
import java.text.ParseException;

import org.apache.commons.io.Charsets;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.shared.io.text.CharsetHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ComparableColumn;
import uk.ac.ucl.excites.sapelli.storage.model.ListLikeColumn;
import uk.ac.ucl.excites.sapelli.storage.util.InvalidValueException;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * A column for Strings
 * 
 * @author mstevens
 */
public class StringColumn extends ComparableColumn<String> implements ListLikeColumn<String>
{
	
	//STATIC---------------------------------------------------------
	static private final long serialVersionUID = 2L;
	
	static public final Charset DEFAULT_CHARSET = Charsets.UTF_8;
	static public final char DEFAULT_SERIALISATION_DELIMITER = '\'';
	
	/**
	 * The number of characters that can fit in the given number of bytes when the given Charset is used to encode them.
	 * Worst case scenario is assumed in which every char needs to maximum number of bytes.
	 * 
	 * @return maximum number of characters that can fit
	 */
	public static int MaximumCharsIn(int allowedBytes, Charset charset)
	{
		return (int) Math.floor(allowedBytes / (double) CharsetHelpers.GetMaxBytesPerChar(charset)); 
	}
	
	/**
	 * The number of bytes needed to encode a String with length up to the given number of characters using the given Charset.
	 * Worst case scenario is assumed in which every char needs to maximum number of bytes.
	 * 
	 * @return number of bytes needed
	 */
	public static int BytesNeededFor(int maxLengthChars, Charset charset)
	{
		return (int) Math.min(	Math.ceil(maxLengthChars * ((double) CharsetHelpers.GetMaxBytesPerChar(charset))),
								Integer.MAX_VALUE);
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param maxLengthChars the maximum length, measured in characters, a String stored in the column will have
	 * @return
	 */
	public static StringColumn ForCharacterCount(String name, boolean optional, int maxLengthChars)
	{
		return ForCharacterCount(name, optional, maxLengthChars, (String) null);
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param maxLengthChars the maximum length, measured in characters, a String stored in the column will have
	 * @param defaultValue
	 * @return
	 */
	public static StringColumn ForCharacterCount(String name, boolean optional, int maxLengthChars, String defaultValue)
	{
		return ForCharacterCount(name, optional, maxLengthChars, DEFAULT_SERIALISATION_DELIMITER);
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param maxLengthChars the maximum length, measured in characters, a String stored in the column will have
	 * @param serialisationDelimiter
	 * @return
	 */
	public static StringColumn ForCharacterCount(String name, boolean optional, int maxLengthChars, char serialisationDelimiter)
	{
		return ForCharacterCount(name, optional, maxLengthChars, (String) null, serialisationDelimiter);
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param maxLengthChars the maximum length, measured in characters, a String stored in the column will have
	 * @param defaultValue
	 * @param serialisationDelimiter
	 * @return
	 */
	public static StringColumn ForCharacterCount(String name, boolean optional, int maxLengthChars, String defaultValue, char serialisationDelimiter)
	{
		return ForCharacterCount(name, optional, maxLengthChars, DEFAULT_CHARSET, defaultValue, serialisationDelimiter);
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param maxLengthChars the maximum length, measured in characters, a String stored in the column will have
	 * @param charset the {@link Charset} to use to encode/decode Strings to/from bytes
	 * @return
	 */
	public static StringColumn ForCharacterCount(String name, boolean optional, int maxLengthChars, Charset charset)
	{
		return ForCharacterCount(name, optional, maxLengthChars, charset, null);
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param maxLengthChars the maximum length, measured in characters, a String stored in the column will have
	 * @param charset the {@link Charset} to use to encode/decode Strings to/from bytes
	 * @param defaultValue
	 * @return
	 */
	public static StringColumn ForCharacterCount(String name, boolean optional, int maxLengthChars, Charset charset, String defaultValue)
	{
		return ForCharacterCount(name, optional, maxLengthChars, charset, defaultValue, DEFAULT_SERIALISATION_DELIMITER);
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param maxLengthChars the maximum length, measured in characters, a String stored in the column will have
	 * @param charset the {@link Charset} to use to encode/decode Strings to/from bytes
	 * @param serialisationDelimiter
	 * @return
	 */
	public static StringColumn ForCharacterCount(String name, boolean optional, int maxLengthChars, Charset charset, char serialisationDelimiter)
	{
		return ForCharacterCount(name, optional, maxLengthChars, charset, null, serialisationDelimiter);
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param maxLengthChars the maximum length, measured in characters, a String stored in the column will have
	 * @param charset the {@link Charset} to use to encode/decode Strings to/from bytes
	 * @param defaultValue
	 * @param serialisationDelimiter
	 * @return
	 */
	public static StringColumn ForCharacterCount(String name, boolean optional, int maxLengthChars, Charset charset, String defaultValue, char serialisationDelimiter)
	{
		if(maxLengthChars <= 0)
			throw new IllegalArgumentException("maxLenghthChars needs to be at least 1 character to make sense, given " + maxLengthChars + " characters");
		return new StringColumn(name, optional, BytesNeededFor(maxLengthChars, charset), charset, defaultValue, serialisationDelimiter);
	}
	
	/**
	 * For upgrade purposes only.
	 * 
	 * @param stringColumn
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.upgrades.Beta17UpgradeStep
	 * @return
	 */
	public static StringColumn Get3BytesPerCharUTF8Version(StringColumn stringColumn)
	{
		if(stringColumn == null || !Charsets.UTF_8.equals(stringColumn.getCharset()))
			return stringColumn;
		// else:
		return new StringColumn(stringColumn.name, stringColumn.optional, stringColumn.getMaximumChars() * 3, Charsets.UTF_8, stringColumn.defaultValue);
	}
	
	//DYNAMIC--------------------------------------------------------
	private final String charsetName;
	private transient Charset charset;
	private final char serialisationDelimiter;
	private final IntegerRangeMapping sizeField;
	
	/**
	 * @param name
	 * @param optional
	 */
	public StringColumn(String name, boolean optional)
	{
		this(name, optional, null);
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param defaultValue
	 */
	public StringColumn(String name, boolean optional, String defaultValue)
	{
		this(name, optional, defaultValue, DEFAULT_SERIALISATION_DELIMITER);
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param serialisationDelimiter
	 */
	public StringColumn(String name, boolean optional, char serialisationDelimiter)
	{
		this(name, optional, (String) null, serialisationDelimiter);
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param defaultValue
	 * @param serialisationDelimiter
	 */
	public StringColumn(String name, boolean optional, String defaultValue, char serialisationDelimiter)
	{
		this(name, optional, BytesNeededFor(Integer.MAX_VALUE /*theoretical max length of Java Strings*/, DEFAULT_CHARSET), DEFAULT_CHARSET, defaultValue, serialisationDelimiter);
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param maxLengthBytes the maximum length (measured in bytes, not chars!) a String stored in this column can have
	 */
	public StringColumn(String name, boolean optional, int maxLengthBytes)
	{
		this(name, optional, maxLengthBytes, (String) null);
	}

	/**
	 * @param name
	 * @param optional
	 * @param maxLengthBytes the maximum length (measured in bytes, not chars!) a String stored in this column can have
	 * @param defaultValue
	 */
	public StringColumn(String name, boolean optional, int maxLengthBytes, String defaultValue)
	{
		this(name, optional, maxLengthBytes, defaultValue, DEFAULT_SERIALISATION_DELIMITER);
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param maxLengthBytes the maximum length (measured in bytes, not chars!) a String stored in this column can have
	 * @param serialisationDelimiter
	 */
	public StringColumn(String name, boolean optional, int maxLengthBytes, char serialisationDelimiter)
	{
		this(name, optional, maxLengthBytes, (String) null, serialisationDelimiter);
	}

	/**
	 * @param name
	 * @param optional
	 * @param maxLengthBytes the maximum length (measured in bytes, not chars!) a String stored in this column can have
	 * @param defaultValue
	 * @param serialisationDelimiter
	 */
	public StringColumn(String name, boolean optional, int maxLengthBytes, String defaultValue, char serialisationDelimiter)
	{
		this(name, optional, maxLengthBytes, DEFAULT_CHARSET, defaultValue, serialisationDelimiter);
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param maxLengthBytes the maximum length (measured in bytes, not chars!) a String stored in this column can have
	 * @param charset the {@link Charset} to use to encode/decode Strings to/from bytes
	 */
	public StringColumn(String name, boolean optional, int maxLengthBytes, Charset charset)
	{
		this(name, optional, maxLengthBytes, charset, null);
	}

	/**
	 * @param name
	 * @param optional
	 * @param maxLengthBytes the maximum length (measured in bytes, not chars!) a String stored in this column can have
	 * @param charset the {@link Charset} to use to encode/decode Strings to/from bytes
	 * @param defaultValue
	 */
	public StringColumn(String name, boolean optional, int maxLengthBytes, Charset charset, String defaultValue)
	{
		this(name, optional, maxLengthBytes, charset, defaultValue, DEFAULT_SERIALISATION_DELIMITER);
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param maxLengthBytes the maximum length (measured in bytes, not chars!) a String stored in this column can have
	 * @param charset the {@link Charset} to use to encode/decode Strings to/from bytes
	 * @param serialisationDelimiter
	 */
	public StringColumn(String name, boolean optional, int maxLengthBytes, Charset charset, char serialisationDelimiter)
	{
		this(name, optional, maxLengthBytes, charset, null, serialisationDelimiter);
	}

	/**
	 * @param name
	 * @param optional
	 * @param maxLengthBytes the maximum length (measured in bytes, not chars!) a String stored in this column can have
	 * @param charset the {@link Charset} to use to encode/decode Strings to/from bytes
	 * @param defaultValue
	 * @param serialisationDelimiter
	 */
	public StringColumn(String name, boolean optional, int maxLengthBytes, Charset charset, String defaultValue, char serialisationDelimiter)
	{
		super(name, optional, defaultValue);
		if(maxLengthBytes <= 0)
			throw new IllegalArgumentException("maxLenghthBytes needs to be at least 1 byte to make sense, given " + maxLengthBytes + " bytes");
		if(charset == null)
			throw new NullPointerException("charset cannot be null!");
		this.charsetName = charset.name(); // !!! (because charset is transient, due to Charset not being Serializable)
		this.charset = charset;
		this.serialisationDelimiter = serialisationDelimiter;
		this.sizeField = new IntegerRangeMapping(0, maxLengthBytes); // empty Strings are allowed
	}
	
	@Override
	public StringColumn createCopy()
	{
		return new StringColumn(name, optional, getMaximumBytes(), Charset.forName(charsetName), defaultValue);
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
	 * @param valueString the String to parse, expected to be neither null nor empty and delimited by {@link #serialisationDelimiter} (meaning that the resulting value is meant to be the empty String, and the serialisationDelimiter is "'", then the given valueString must be "''")
	 * @return the parsed value
	 * 
	 * @see #toString(String)
	 */
	@Override
	public String parse(String valueString) throws ParseException
	{
		return parse(valueString, false); // delimited!
	}

	/**
	 * If {@code undelimited} is {@code false} the given {@code valueString} is expected to be wrapped/escaped using {@link #serialisationDelimiter}s.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.ListLikeColumn#parse(java.lang.String, boolean)
	 * @see #toString(String, boolean)
	 */
	@Override
	public String parse(String valueString, boolean undelimited) throws ParseException, IllegalArgumentException, NullPointerException
	{
		if(!undelimited)
		{
			// Perform delimiter checks:
			if(valueString.length() < 2)
				throw new ParseException("String is not delimited by " + serialisationDelimiter + "s", 0);
			if(valueString.charAt(0) != serialisationDelimiter)
				throw new ParseException("String does not begin with " + serialisationDelimiter, 0);
			if(valueString.charAt(valueString.length() - 1) != serialisationDelimiter)
				throw new ParseException("String does not end with " + serialisationDelimiter, valueString.length() - 1);
			// Remove serialisationDelimiters:
			return StringUtils.deescapeByDoublingAndWrapping(valueString, serialisationDelimiter);
		}
		else
			return valueString;
	}
	
	/**
	 * The given value will be wrapped/escaped using {@link #serialisationDelimiter}s to preserve the difference between a null String and an empty String value (because empty Strings are treated as null in {@link Column})
	 * Occurrences of the delimiter *inside* the value will be doubled. 
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#toString(java.lang.Object)
	 */
	@Override
	public String toString(String value)
	{
		return toString(value, false); // delimited!
	}

	/**
	 * If {@code undelimited} is {@code false} given value will be wrapped/escaped using {@link #serialisationDelimiter}s to preserve the difference between a null String and an empty String value (because empty Strings are treated as null in {@link Column}).
	 * Occurrences of the delimiter *inside* the value will then be doubled.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.ListLikeColumn#toString(java.lang.Object, boolean)
	 */
	@Override
	public String toString(String value, boolean undelimited)
	{
		return undelimited ? value : StringUtils.escapeByDoublingAndWrapping(value, serialisationDelimiter, /*force:*/ true);
	}
	
	public boolean fits(String value)
	{
		return isValidValue(value);
	}
	
	/**
	 * Checks for size restriction violations
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#validate(java.lang.Object)
	 */
	@Override
	protected void validate(String value) throws InvalidValueException
	{
		int bytesNeeded = StringUtils.sizeBytes(value, getCharset());
		if(bytesNeeded > getMaximumBytes())
			throw new InvalidValueException("String \"" + value + "\" is too long (it would take " + bytesNeeded + " bytes, while the maximum allowed is " + getMaximumBytes() + " bytes).", this);
	}

	@Override
	protected void write(String value, BitOutputStream bitStream, boolean lossless) throws IOException
	{
		// Write length:
		sizeField.write(StringUtils.sizeBytes(value, getCharset()), bitStream);
		// Write actual string:
		bitStream.write(value, getCharset());
	}

	@Override
	protected String read(BitInputStream bitStream, boolean lossless) throws IOException
	{
		//Read length:
		int numberOfBytes = sizeField.read(bitStream).intValue();
		//Read actual string:
		return bitStream.readString(numberOfBytes, getCharset());
	}

	@Override
	protected int getMinimumValueSize(boolean lossless)
	{
		return sizeField.size(); // when stored string is empty: just the size field
	}
	
	@Override
	protected int getMaximumValueSize(boolean lossless)
	{
		return sizeField.size() + (getMaximumBytes() * Byte.SIZE);
	}
	
	public int getMaximumBytes()
	{
		return sizeField.highBound().intValue();
	}
	
	/**
	 * Worst case scenario is assumed in which every char needs to maximum number of bytes
	 * 
	 * @return
	 */
	public int getMaximumChars()
	{
		return MaximumCharsIn(getMaximumBytes(), getCharset());
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#canBeLossy()
	 */
	@Override
	public boolean canBeLossy()
	{
		return false;
	}

	@Override
	protected boolean equalRestrictions(Column<String> otherColumn)
	{
		if(otherColumn instanceof StringColumn)
		{
			StringColumn that = (StringColumn) otherColumn;
			return	this.getMaximumBytes() == that.getMaximumBytes() &&
					this.serialisationDelimiter == that.serialisationDelimiter &&
					this.getCharset().equals(that.getCharset());
		}
		else
			return false;
	}

	@Override
	protected String copy(String value)
	{
		return new String(value);
	}

	@Override
	public void accept(ColumnVisitor visitor)
	{
		visitor.visit(this);
	}

	@Override
	protected int compareNonNullValues(String lhs, String rhs)
	{
		return lhs.compareTo(rhs);
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode();
		hash = 31 * hash + getCharset().hashCode();
		hash = 31 * hash + serialisationDelimiter;
		hash = 31 * hash + sizeField.hashCode();
		return hash;
	}
	
	public Charset getCharset()
	{
		if(this.charset == null)
			this.charset = Charset.forName(charsetName); // needed because charset member variable is transient (because Charset is not a Serialisable class)
		return charset;
	}

	@Override
	public Class<String> getType()
	{
		return String.class;
	}
	
}
