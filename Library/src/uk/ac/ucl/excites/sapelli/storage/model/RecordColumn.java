/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model;

import java.io.IOException;
import java.text.ParseException;

import uk.ac.ucl.excites.sapelli.storage.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.storage.io.BitOutputStream;

/**
 * A composite column, consisting of a set of "subcolumns" as specified by a Schema
 * 
 * @author mstevens
 */
public abstract class RecordColumn<R extends Record> extends Column<R>
{
	
	static public final String DEFAULT_SUBVALUE_SEPARATOR = "#";
	
	protected Schema schema;

	public RecordColumn(Class<R> type, String name, Schema schema, boolean optional)
	{
		super(type, name, optional);
		this.schema = schema;
	}
	
	/**
	 * Subclasses may override this
	 * 
	 * @return
	 */
	public String getSubvalueSeparator()
	{
		return DEFAULT_SUBVALUE_SEPARATOR;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#parse(java.lang.String)
	 * 
	 * TODO escaping!
	 */
	@Override
	protected R parse(String recordStr) throws ParseException, IllegalArgumentException, NullPointerException
	{
		String[] parts = recordStr.split(getSubvalueSeparator());
		R record = getNewRecord();
		if(parts.length != schema.getNumberOfColumns())
			throw new IllegalArgumentException("Mismatch in number of subvalues, got " + parts.length + ", expecting " + schema.getNumberOfColumns() + ".");
		int p = 0;
		for(Column<?> subCol : schema.getColumns())
			subCol.parseAndStoreValue(record, parts[p++]);
		return record;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#toString(java.lang.Object)
	 * 
	 * TODO escaping!
	 * TODO might need to change is we change behaviour of empty string
	 */
	@Override
	protected String toString(R record)
	{
		StringBuilder bldr = new StringBuilder();
		boolean first = true;
		for(Column<?> subCol : schema.getColumns())
		{
			String subValueString = subCol.retrieveValueAsString(record);
			if(first)
			{
				bldr.append(getSubvalueSeparator());
				first = false;
			}
			bldr.append(subValueString == null ? "" : subValueString);
		}
		return bldr.toString();
	}

	@Override
	protected void write(R record, BitOutputStream bitStream) throws IOException
	{
		for(Column<?> subCol : schema.getColumns())
			subCol.writeObject(subCol.retrieveValue(record), bitStream); // will also write optional bit of the subcolumn if it is optional
	}
	
	protected abstract R getNewRecord();

	@Override
	protected R read(BitInputStream bitStream) throws IOException
	{
		R record = getNewRecord();
		for(Column<?> subCol : schema.getColumns())
			subCol.storeObject(record, subCol.readValue(bitStream));
		return record;
	}

	@Override
	protected void validate(R record) throws IllegalArgumentException
	{
		for(Column<?> subCol : schema.getColumns())
			subCol.validateObject(subCol.retrieveValue(record));
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#_getMaximumSize()
	 */
	@Override
	protected int _getMaximumSize()
	{
		return schema.getMaximumSize();
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#_getMinimumSize()
	 */
	@Override
	protected int _getMinimumSize()
	{
		return schema.getMinimumSize();
	}

}
