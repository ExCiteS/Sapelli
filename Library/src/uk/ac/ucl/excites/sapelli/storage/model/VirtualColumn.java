/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model;

import java.io.IOException;
import java.io.Serializable;
import java.text.ParseException;

import uk.ac.ucl.excites.sapelli.storage.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.storage.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * A "virtual", read-only(!) column which maps the values of a "real" column (the sourceColumn)
 * to a different type/representation, which can be handled by another targetColumn.
 * The sourceColumn is assumed to be part of a schema on its own, the target column should never be added to a schema directly.
 * 
 * @author mstevens
 */
public class VirtualColumn<VT, ST> extends Column<VT>
{
	
	private static final long serialVersionUID = 2L;
	private static final char NAME_SEPARATOR = '-';

	private final Column<ST> sourceColumn; // the "real" column
	private final Column<VT> targetColumn; // the "virtual" column
	private final ValueMapper<VT, ST> valueMapper;

	public VirtualColumn(Column<ST> sourceColumn, Column<VT> targetColumn, ValueMapper<VT, ST> valueMapper)
	{
		super(targetColumn.getType(), sourceColumn.name + NAME_SEPARATOR + targetColumn.name, targetColumn.optional);
		if(sourceColumn == null || targetColumn == null || valueMapper == null)
			throw new NullPointerException("sourceColumn, targetColumn & valueMapper cannot be null!");
		if(sourceColumn == targetColumn)
			throw new IllegalArgumentException("source- & targetColumn cannot be the same!");
		if(sourceColumn.optional != targetColumn.optional)
			throw new IllegalArgumentException("Optionality of target & source column must be the same!");
		this.sourceColumn = sourceColumn;
		this.targetColumn = targetColumn;
		this.valueMapper = valueMapper;
	}
	
	@Override
	public VirtualColumn<VT, ST> copy()
	{
		return new VirtualColumn<VT, ST>(sourceColumn, targetColumn.copy(), valueMapper);
	}
	
	@Override
	public void storeValue(Record record, VT value) throws IllegalArgumentException, NullPointerException, UnsupportedOperationException
	{
		throw new UnsupportedOperationException("VirtualColumns are read-only!");
	}
	
	/**
	 * Overridden to retrieve value from sourceColum and convert it.
	 *  
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#retrieveValue(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	public VT retrieveValue(Record record)
	{
		// Retrieve value from sourceColumn:
		ST sourceValue = sourceColumn.retrieveValue(record);
		if(sourceValue == null)
			return null;
		// Return converted value:
		return valueMapper.mapValue(sourceValue);
	}

	@Override
	public VT parse(String value) throws ParseException, IllegalArgumentException, NullPointerException
	{
		return targetColumn.parse(value);
	}

	@Override
	public String toString(VT value)
	{
		return targetColumn.toString(value);
	}

	@Override
	protected void write(VT value, BitOutputStream bitStream) throws IOException
	{
		targetColumn.write(value, bitStream);
	}

	@Override
	protected VT read(BitInputStream bitStream) throws IOException
	{
		return targetColumn.read(bitStream);
	}

	@Override
	protected void validate(VT value) throws IllegalArgumentException
	{
		targetColumn.validate(value);
	}

	@Override
	protected VT copy(VT value)
	{
		return targetColumn.copy(value);
	}

	@Override
	protected int _getMaximumSize()
	{
		return targetColumn._getMaximumSize();
	}

	@Override
	protected int _getMinimumSize()
	{
		return targetColumn._getMinimumSize();
	}

	@Override
	protected boolean equalRestrictions(Column<VT> otherColumn)
	{
		if(this.getClass().isInstance(otherColumn))
		{
			VirtualColumn<?, ?> otherVirtualColumn = (VirtualColumn<?, ?>) otherColumn;
			return 	this.targetColumn.equals(otherVirtualColumn.targetColumn) &&
					this.valueMapper.equals(otherVirtualColumn.valueMapper);
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
	
	public Column<VT> getTargetColumn()
	{
		return targetColumn;
	}
	
	/**
	 * @author mstevens
	 *
	 * @param <VT>
	 * @param <ST>
	 */
	static public abstract class ValueMapper<VT, ST> implements Serializable
	{
	
		private static final long serialVersionUID = 2L;

		public abstract VT mapValue(ST nonNullValue);
		
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

}
