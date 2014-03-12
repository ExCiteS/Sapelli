/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model;

import java.io.IOException;
import java.text.ParseException;
import java.util.Set;
import java.util.TreeSet;

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
	
	protected final Schema schema;
	protected final Set<Column<?>> skipColumns;

	public RecordColumn(Class<R> type, String name, Schema schema, boolean optional)
	{
		super(type, name, optional);
		this.schema = schema;
		this.skipColumns = new TreeSet<Column<?>>();
	}
	
	protected void addSkipColumn(Column<?> skipColumn)
	{
		skipColumns.add(skipColumn);
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
	 */
	@Override
	protected R parse(String recordStr) throws ParseException, IllegalArgumentException, NullPointerException
	{
		R record = getNewRecord();
		record.parse(recordStr, skipColumns);
		return record;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#toString(java.lang.Object)
	 */
	@Override
	protected String toString(R record)
	{
		return record.serialise(skipColumns);
	}

	@Override
	protected void write(R record, BitOutputStream bitStream) throws IOException
	{
		for(Column<?> subCol : schema.getColumns())
			if(!skipColumns.contains(subCol))
				subCol.writeObject(subCol.retrieveValue(record), bitStream); // will also write optional bit of the subcolumn if it is optional
	}
	
	protected abstract R getNewRecord();

	@Override
	protected R read(BitInputStream bitStream) throws IOException
	{
		R record = getNewRecord();
		for(Column<?> subCol : schema.getColumns())
			if(!skipColumns.contains(subCol))
				subCol.storeObject(record, subCol.readValue(bitStream));
		return record;
	}

	@Override
	protected void validate(R record) throws IllegalArgumentException
	{
		for(Column<?> subCol : schema.getColumns())
			if(!skipColumns.contains(subCol))
				subCol.validateObject(subCol.retrieveValue(record));
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#_getMaximumSize()
	 */
	@Override
	protected int _getMaximumSize()
	{
		return schema.getMaximumSize(skipColumns);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#_getMinimumSize()
	 */
	@Override
	protected int _getMinimumSize()
	{
		return schema.getMinimumSize(skipColumns);
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#equalRestrictions(uk.ac.ucl.excites.sapelli.storage.model.Column)
	 */
	@Override
	protected boolean equalRestrictions(Column<R> otherColumn)
	{
		if(otherColumn instanceof RecordColumn)
		{
			RecordColumn<R> otherRecCol = (RecordColumn<R>) otherColumn; 
			return schema.equals(otherRecCol.schema) && skipColumns.containsAll(otherRecCol.skipColumns) && otherRecCol.skipColumns.containsAll(skipColumns);
		}
		return false;
	}

}
