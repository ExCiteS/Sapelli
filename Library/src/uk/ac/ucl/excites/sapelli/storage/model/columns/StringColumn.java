package uk.ac.ucl.excites.sapelli.storage.model.columns;
/**
 * 
 */

import java.io.IOException;
import java.nio.charset.Charset;
import java.text.ParseException;

import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.storage.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.storage.io.BitOutputStream;
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
	
	static private final Charset DEFAULT_CHARSET = Charset.forName("UTF-8");
	static private final int DEFAULT_MAX_LENGTH_BYTES = 256; //bytes
	static private final String SERIALISATION_QUOTE = "'";
	
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
		return new StringColumn(name, optional, (int) Math.ceil(maxLengthChars * charset.newEncoder().maxBytesPerChar()), charset);
	}
	
	//DYNAMIC--------------------------------------------------------
	private Charset charset;	
	private IntegerRangeMapping sizeField;
	
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
		this.charset = charset;
		this.sizeField = new IntegerRangeMapping(0, maxLengthBytes); // empty Strings are allowed
	}
	
	@Override
	public StringColumn copy()
	{
		return new StringColumn(name, optional, getMaximumBytes(), charset);
	}
	
	/**
	 * @param value the String to parse, expected to be neither null nor "" and delimited by {@link StringColumn#SERIALISATION_QUOTE} (meaning that if it is meant to be an empty String it will be "''")
	 * @return the parsed value
	 */
	@Override
	public String parse(String value) throws ParseException
	{
		if(!value.startsWith(SERIALISATION_QUOTE))
			throw new ParseException("String is not delimited by " + SERIALISATION_QUOTE + "s", 0);
		if(!value.endsWith(SERIALISATION_QUOTE))
			throw new ParseException("String is not delimited by " + SERIALISATION_QUOTE + "s", value.length() - 1);
		return value.substring(1, value.length() - 1); // strip away the quotes
	}
	
	/**
	 * We wrap all Strings between {@link StringColumn#SERIALISATION_QUOTE}s to avoid that empty Strings are treated as null in {@link Column} and elsewhere
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#toString(java.lang.Object)
	 */
	@Override
	public String toString(String value)
	{
		return SERIALISATION_QUOTE + value + SERIALISATION_QUOTE; // surround with quotes
	}

	/**
	 * Checks for size restriction violations
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#validate(java.lang.Object)
	 */
	@Override
	protected void validate(String value) throws IllegalArgumentException
	{
		int bytesNeeded = StringUtils.sizeBytes(value, charset);
		if(bytesNeeded > getMaximumBytes())
			throw new IllegalArgumentException("String \"" + value + "\" is too long (it would take " + bytesNeeded + " bytes, while the maximum allowed is " + getMaximumBytes() + " bytes).");
	}

	@Override
	protected void write(String value, BitOutputStream bitStream) throws IOException
	{
		//Write length:
		sizeField.write(StringUtils.sizeBytes(value, charset), bitStream);
		//Write actual string:
		bitStream.write(value, charset);
	}

	@Override
	protected String read(BitInputStream bitStream) throws IOException
	{
		//Read length:
		int numberOfBytes = (int) sizeField.read(bitStream);
		//Read actual string:
		return bitStream.readString(numberOfBytes, charset);
	}

	@Override
	protected int _getMinimumSize()
	{
		return sizeField.getSize(); // when stored string is empty: just the size field
	}
	
	@Override
	protected int _getMaximumSize()
	{
		return sizeField.getSize() + getMaximumBytes() * Byte.SIZE;
	}
	
	public int getMaximumBytes()
	{
		return (int) sizeField.getHighBound();
	}
	
	/**
	 * Worst case scenario is assumed in which every char needs to maximum number of bytes
	 * 
	 * @return
	 */
	public int getMaximumChars()
	{
		return (int) Math.floor(getMaximumBytes() / charset.newEncoder().maxBytesPerChar()); 
	}
	
	@Override
	protected boolean equalRestrictions(Column<String> otherColumn)
	{
		if(otherColumn instanceof StringColumn)
		{
			StringColumn other = (StringColumn) otherColumn;
			return this.getMaximumBytes() == other.getMaximumBytes() && this.charset.equals(other.charset);
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
		hash = 31 * hash + charset.hashCode();
		hash = 31 * hash + sizeField.hashCode();
		return hash;
	}
	
}
