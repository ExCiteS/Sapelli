/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model.columns;

import java.io.IOException;
import java.text.ParseException;
import java.util.Arrays;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * Column to store byte arrays
 * 
 * TODO add compression support?
 * 
 * @author mstevens
 */
public class ByteArrayColumn extends Column<byte[]>
{

	static private final long serialVersionUID = 2L;
	
	static public final char SERIALISATION_SEPARATOR = ',';
	
	private final IntegerRangeMapping sizeField;
	
	/**
	 * @param type
	 * @param name
	 * @param optional
	 */
	public ByteArrayColumn(String name, boolean optional)
	{
		super(byte[].class, name, optional);
		this.sizeField = new IntegerRangeMapping(0, Integer.MAX_VALUE);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#copy()
	 */
	@Override
	public Column<byte[]> copy()
	{
		return new ByteArrayColumn(name, optional);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#parse(java.lang.String)
	 */
	@Override
	public byte[] parse(String valueStr) throws ParseException, IllegalArgumentException, NullPointerException
	{
		String[] parts = valueStr.split("\\" + SERIALISATION_SEPARATOR);
		byte[] bytes = new byte[parts.length];
		for(int i=0, len=bytes.length; i<len; i++)
			bytes[i] = Byte.parseByte(parts[i].trim());
		return bytes;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#toString(java.lang.Object)
	 */
	@Override
	public String toString(byte[] value)
	{
		StringBuilder bldr = new StringBuilder();
		boolean first = true;
		for(byte b : value)
		{
			// Separator:
			if(first)
				first = false;
			else
				bldr.append(SERIALISATION_SEPARATOR);
			// Value:
			if(value != null)
				bldr.append(Byte.toString(b));
		}
		return bldr.toString();
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#write(java.lang.Object, uk.ac.ucl.excites.sapelli.storage.io.BitOutputStream)
	 */
	@Override
	protected void write(byte[] value, BitOutputStream bitStream) throws IOException
	{
		//Write length:
		sizeField.write(value.length, bitStream);
		//Write actual string:
		bitStream.write(value);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#read(uk.ac.ucl.excites.sapelli.storage.io.BitInputStream)
	 */
	@Override
	protected byte[] read(BitInputStream bitStream) throws IOException
	{
		//Read length:
		int numberOfBytes = (int) sizeField.read(bitStream);
		//Read actual bytes:
		return bitStream.readBytes(numberOfBytes);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#validate(java.lang.Object)
	 */
	@Override
	protected void validate(byte[] value) throws IllegalArgumentException
	{
		// does nothing
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#copy(java.lang.Object)
	 */
	@Override
	protected byte[] copy(byte[] value)
	{
		return Arrays.copyOf(value, value.length);
	}

	@Override
	protected int _getMinimumSize()
	{
		return sizeField.getSize(); // when stored array has 0 lengthy: just the size field is stored
	}
	
	@Override
	protected int _getMaximumSize()
	{
		return sizeField.getSize() + ((int) sizeField.getHighBound()) * Byte.SIZE;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#equalRestrictions(uk.ac.ucl.excites.sapelli.storage.model.Column)
	 */
	@Override
	protected boolean equalRestrictions(Column<byte[]> otherColumn)
	{
		return otherColumn instanceof ByteArrayColumn;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#accept(uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor)
	 */
	@Override
	public void accept(ColumnVisitor visitor)
	{
		visitor.visit(this);
	}
	
	@Override
    public int hashCode()
	{
		int hash = super.hashCode();
		hash = 31 * hash + sizeField.hashCode();
		return hash;
	}

}
