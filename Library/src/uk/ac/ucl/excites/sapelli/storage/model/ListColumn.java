/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model;

import java.io.IOException;
import java.text.ParseException;
import java.util.List;

import uk.ac.ucl.excites.sapelli.storage.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.storage.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.util.StringUtils;

/**
 * @author mstevens
 *
 */
public abstract class ListColumn<T> extends Column<List<T>>
{
	
	static public final int DEFAULT_MINIMUM_LENGTH = 0; // list items
	static public final char SERIALISATION_SEPARATOR = ',';
	static public final char SERIALISATION_SEPARATOR_ESCAPE = '.';
	static public final char SERIALISATION_SEPARATOR_ESCAPE_PREFIX = '/';
	
	static protected int GetMaxLengthForSizeFieldSize(int minLength, int sizeBits)
	{
		return (int) IntegerRangeMapping.ForSize(minLength, sizeBits).getHighBound();
	}
	
	private final IntegerRangeMapping sizeField;
	protected final Column<T> singleColumn;
	
	public ListColumn(String name, Column<T> singleColumn, boolean optional, int maxLength)
	{
		this(name, singleColumn, optional, DEFAULT_MINIMUM_LENGTH, maxLength);
	}
	
	@SuppressWarnings("unchecked")
	public ListColumn(String name, Column<T> singleColumn, boolean optional, int minLength, int maxLength)
	{
		super((Class<List<T>>)(Class<?>)List.class, name, optional);
		this.singleColumn = singleColumn;
		this.sizeField = new IntegerRangeMapping(minLength, maxLength);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#parse(java.lang.String)
	 */
	@Override
	protected List<T> parse(String recordStr) throws ParseException, IllegalArgumentException, NullPointerException
	{
		String[] parts = recordStr.split("\\" + SERIALISATION_SEPARATOR);
		List<T> values = getNewList(parts.length);
		for(String v : parts)
			values.add(singleColumn.parse(StringUtils.deescape(v, SERIALISATION_SEPARATOR, SERIALISATION_SEPARATOR_ESCAPE, SERIALISATION_SEPARATOR_ESCAPE_PREFIX)));
		return values;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#toString(java.lang.Object)
	 */
	@Override
	protected String toString(List<T> values)
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

	@Override
	protected void write(List<T> values, BitOutputStream bitStream) throws IOException
	{
		// Write size:
		sizeField.write(values.size(), bitStream);
		// Write values:
		for(T value : values)
			singleColumn.writeValue(value, bitStream);
	}
	
	protected abstract List<T> getNewList(int minimumCapacity);

	@Override
	protected List<T> read(BitInputStream bitStream) throws IOException
	{
		// Read size:
		int size = (int) sizeField.read(bitStream);
		// Read values:
		List<T> values = getNewList(size);
		for(int i = 0; i < size; i++)
			values.add(singleColumn.readValue(bitStream));
		return values;
	}

	@Override
	protected void validate(List<T> values) throws IllegalArgumentException
	{
		if(values.size() < sizeField.getLowBound())
			throw new IllegalArgumentException(getTypeString() + " does not contain enough " + singleColumn.getTypeString() + "s, minimum is " + sizeField.getLowBound() + ", given value has " + values.size() + ".");
		if(values.size() > sizeField.getHighBound())
			throw new IllegalArgumentException(getTypeString() + " contains too many " + singleColumn.getTypeString() + "s, maximum is " + sizeField.getLowBound() + ", given value has " + values.size() + ".");
	}

	@Override
	protected List<T> copy(List<T> values)
	{
		List<T> copy = getNewList(values.size());
		for(T value : values)
			copy.add(singleColumn.copy(value));
		return copy;
	}

	@Override
	protected int _getMaximumSize()
	{
		return sizeField.getSize() + (getMaximumLength() * singleColumn.getMaximumSize());
	}

	@Override
	protected int _getMinimumSize()
	{
		return sizeField.getSize() + (getMinimumLength() * singleColumn.getMinimumSize());
	}

	@Override
	protected boolean equalRestrictions(Column<List<T>> otherColumn)
	{
		if(otherColumn instanceof ListColumn)
		{
			ListColumn<T> other = (ListColumn<T>) otherColumn;
			return this.singleColumn.equals(other.singleColumn) && this.sizeField.getHighBound() == other.sizeField.getHighBound();
		}
		return false;
	}

	/**
	 * @return the maxLength
	 */
	public int getMinimumLength()
	{
		return (int) sizeField.getLowBound();
	}
	
	/**
	 * @return the maxLength
	 */
	public int getMaximumLength()
	{
		return (int) sizeField.getHighBound();
	}

	@Override
	public String getTypeString()
	{
		return singleColumn.getTypeString() + List.class.getSimpleName();
	}

}
