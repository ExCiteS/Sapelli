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

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.shared.util.UnicodeHelpers;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ComparatorColumn;
import uk.ac.ucl.excites.sapelli.storage.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * A column for Strings
 * 
 * @author mstevens
 */
public class StringColumn extends ComparatorColumn<String>
{
	
	//STATIC---------------------------------------------------------
	static private final long serialVersionUID = 2L;
	
	static private final Charset DEFAULT_CHARSET = UnicodeHelpers.UTF8;
	static private final int DEFAULT_MAX_LENGTH_BYTES = 256; //bytes
	static private final char SERIALISATION_QUOTE = '\'';
	
	/**
	 * The number of characters that can fit in the given number of bytes when the given Charset is used to encode them.
	 * Worst case scenario is assumed in which every char needs to maximum number of bytes.
	 * 
	 * @return maximum number of characters that can fit
	 */
	public static int MaximumCharsIn(int allowedBytes, Charset charset)
	{
		return (int) Math.floor(allowedBytes / charset.newEncoder().maxBytesPerChar()); 
	}
	
	/**
	 * The number of bytes needed to encode a String with length up to the given number of characters using the given Charset.
	 * Worst case scenario is assumed in which every char needs to maximum number of bytes.
	 * 
	 * @return number of bytes needed
	 */
	public static int BytesNeededFor(int maxLengthChars, Charset charset)
	{
		return (int) Math.ceil(maxLengthChars * charset.newEncoder().maxBytesPerChar());
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param maxLengthChars the maximum length, measured in characters, a String stored in the column will have
	 * @return
	 */
	public static StringColumn ForCharacterCount(String name, boolean optional, int maxLengthChars)
	{
		return ForCharacterCount(name, optional, maxLengthChars, DEFAULT_CHARSET);
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param maxLengthChars the maximum length, measured in characters, a String stored in the column will have
	 * @param charset the charset to use to encode/decode Strings to/from bytes
	 * @return
	 */
	public static StringColumn ForCharacterCount(String name, boolean optional, int maxLengthChars, Charset charset)
	{
		if(maxLengthChars <= 0)
			throw new IllegalArgumentException("maxLenghthChars needs to be at least 1 character to make sense, given " + maxLengthChars + " characters");
		return new StringColumn(name, optional, BytesNeededFor(maxLengthChars, charset), charset);
	}
	
	//DYNAMIC--------------------------------------------------------
	private final String charsetName;
	private transient Charset charset;
	private final IntegerRangeMapping sizeField;
	
	/**
	 * @param name
	 * @param optional
	 */
	public StringColumn(String name, boolean optional)
	{
		this(name, optional, DEFAULT_MAX_LENGTH_BYTES, DEFAULT_CHARSET);
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param maxLengthBytes the maximum length (measured in bytes, not chars!) a String stored in this column can have
	 */
	public StringColumn(String name, boolean optional, int maxLengthBytes)
	{
		this(name, optional, maxLengthBytes, DEFAULT_CHARSET);
	}
	
	/**
	 * @param name
	 * @param optional
	 * @param maxLengthBytes the maximum length (measured in bytes, not chars!) a String stored in this column can have
	 * @param charset the charset to use to encode/decode Strings to/from bytes
	 */
	public StringColumn(String name, boolean optional, int maxLengthBytes, Charset charset)
	{
		super(String.class, name, optional);
		if(maxLengthBytes <= 0)
			throw new IllegalArgumentException("maxLenghthBytes needs to be at least 1 byte to make sense, given " + maxLengthBytes + " bytes");
		if(charset == null)
			throw new NullPointerException("charset cannot be null!");
		this.charsetName = charset.name(); // !!! (because charset is transient, due to Charset not being Serializable)
		this.charset = charset;
		this.sizeField = new IntegerRangeMapping(0, maxLengthBytes); // empty Strings are allowed
	}
	
	@Override
	public StringColumn copy()
	{
		return new StringColumn(name, optional, getMaximumBytes(), Charset.forName(charsetName));
	}
	
	/**
	 * @param value the String to parse, expected to be neither null nor "" and delimited by {@link StringColumn#SERIALISATION_QUOTE} (meaning that if it is meant to be an empty String it will be "''")
	 * @return the parsed value
	 */
	@Override
	public String parse(String value) throws ParseException
	{
		if(value.length() < 2)
			throw new ParseException("String is not delimited by " + SERIALISATION_QUOTE + "s", 0);
		if(value.charAt(0) != SERIALISATION_QUOTE)
			throw new ParseException("String is not delimited by " + SERIALISATION_QUOTE + "s", 0);
		if(value.charAt(value.length() - 1) != SERIALISATION_QUOTE)
			throw new ParseException("String is not delimited by " + SERIALISATION_QUOTE + "s", value.length() - 1);
		return value.substring(1, value.length() - 1); // strip away the quotes
	}
	
	/**
	 * We wrap all Strings between {@link SERIALISATION_QUOTE}s to avoid that empty Strings are treated as null in {@link Column} and elsewhere
	 * 
	 * Warning, no escape takes place at this level! So occurrences of {@link SERIALISATION_QUOTE} within the value will be left unchanged.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#toString(java.lang.Object)
	 */
	@Override
	public String toString(String value)
	{
		return SERIALISATION_QUOTE + value + SERIALISATION_QUOTE; // surround with quotes
		//TODO escape!!!!!
	}
	
	/**
	 * Checks for size restriction violations
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#validate(java.lang.Object)
	 */
	@Override
	protected void validate(String value) throws IllegalArgumentException
	{
		int bytesNeeded = StringUtils.sizeBytes(value, getCharset());
		if(bytesNeeded > getMaximumBytes())
			throw new IllegalArgumentException("String \"" + value + "\" is too long (it would take " + bytesNeeded + " bytes, while the maximum allowed is " + getMaximumBytes() + " bytes).");
	}

	@Override
	protected void write(String value, BitOutputStream bitStream) throws IOException
	{
		//Write length:
		sizeField.write(StringUtils.sizeBytes(value, getCharset()), bitStream);
		//Write actual string:
		bitStream.write(value, getCharset());
	}

	@Override
	protected String read(BitInputStream bitStream) throws IOException
	{
		//Read length:
		int numberOfBytes = (int) sizeField.read(bitStream);
		//Read actual string:
		return bitStream.readString(numberOfBytes, getCharset());
	}

	@Override
	protected int _getMinimumSize()
	{
		return sizeField.size(); // when stored string is empty: just the size field
	}
	
	@Override
	protected int _getMaximumSize()
	{
		return sizeField.size() + getMaximumBytes() * Byte.SIZE;
	}
	
	public int getMaximumBytes()
	{
		return (int) sizeField.highBound();
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
	
	@Override
	protected boolean equalRestrictions(Column<String> otherColumn)
	{
		if(otherColumn instanceof StringColumn)
		{
			StringColumn other = (StringColumn) otherColumn;
			return this.getMaximumBytes() == other.getMaximumBytes() && this.getCharset().equals(other.getCharset());
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
		hash = 31 * hash + sizeField.hashCode();
		return hash;
	}
	
	public Charset getCharset()
	{
		if(this.charset == null)
			this.charset = Charset.forName(charsetName); // needed because charset member variable is transient (because Charset is not a Serialisable class)
		return charset;
	}
	
}
