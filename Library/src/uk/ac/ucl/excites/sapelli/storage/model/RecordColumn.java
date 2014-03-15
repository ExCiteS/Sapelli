/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.model;

import java.io.IOException;
import java.text.ParseException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.storage.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.storage.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.util.CollectionUtils;

/**
 * A composite column, consisting of a set of "subcolumns" as specified by a Schema
 *
 * The "skip columns" mechanism allows certain subcolumns to be skipped when
 * writing/reading records to/from binary storage. Optionally this mechanism can also
 * be applied upon serialisation/deserialisation to/from String, but the default
 * behaviour is to include them.
 * 
 * The "swap columns" mechanism allows to deal with the situation in which (some) columns
 * of the Schema used for Record instances are not the same as those in the Schema used by
 * the RecordColumn instance to hold its subcolumns. Currently only used in {@link LocationColumn}.
 * 
 * @author mstevens
 */
public abstract class RecordColumn<R extends Record> extends Column<R>
{

	static public final boolean DEFAULT_FULL_SERIALISATION = true; // don't skip columns upon (de)serialisation by default
	
	protected final Schema schema;
	protected final Set<Column<?>> skipColumns;
	protected final boolean fullSerialisation;
	private final Map<Column<?>, Column<?>> swapColumns;

	public RecordColumn(Class<R> type, String name, Schema schema, boolean optional)
	{
		this(type, name, schema, optional, DEFAULT_FULL_SERIALISATION);
	}
	
	/**
	 * @param type
	 * @param name
	 * @param schema	schema containing subcolumns
	 * @param optional
	 * @param fullSerialisation	whether serialisation/deserialisation should exclude the subcolumns in skipcolumns (false) or not (true)
	 */
	public RecordColumn(Class<R> type, String name, Schema schema, boolean optional, boolean fullSerialisation)
	{
		super(type, name, optional);
		this.schema = schema;
		this.skipColumns = new HashSet<Column<?>>();
		this.fullSerialisation = fullSerialisation;
		this.swapColumns = new HashMap<Column<?>, Column<?>>();
	}
	
	/**
	 * Add a column that needs to be skipped upon reading/writing to binary storage and
	 * optionally also for serialisation/deserialisation to String
	 * 
	 * @param skipColumn
	 */
	protected void addSkipColumn(Column<?> skipColumn)
	{
		if(!schema.containsColumn(skipColumn))
			throw new IllegalArgumentException("Unknown subColumn");
		skipColumns.add(skipColumn);
	}
	
	/**
	 * Specify a column instance to be used instead of one of the subcolumns
	 * when storing/retrieving values from records.
	 * The recordColumn must be other same content type as the subColumn,
	 * but no names, optionality or other other restrictions are checked.
	 * Use with care!
	 * 
	 * @param subColumn
	 * @param recordColumn
	 */
	protected void addRecordColumn(Column<?> subColumn, Column<?> recordColumn)
	{
		if(!schema.containsColumn(subColumn))
			throw new IllegalArgumentException("Unknown subColumn");
		if(!subColumn.getType().equals(recordColumn.getType()))
			throw new IllegalArgumentException("Column type mismatch!");
		swapColumns.put(subColumn, recordColumn);
	}
	
	/**
	 * Get column to store/retrieve values in record objects.
	 * This is the given subColumn itself, unless a "recordColumn" has been registered for it.
	 * 
	 * @param subColumn
	 * @return
	 */
	private Column<?> getRecordColumn(Column<?> subColumn)
	{
		if(swapColumns.containsKey(subColumn))
			return swapColumns.get(subColumn);
		else
			return subColumn;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#parse(java.lang.String)
	 */
	@Override
	protected R parse(String recordStr) throws ParseException, IllegalArgumentException, NullPointerException
	{
		return parse(recordStr, fullSerialisation);
	}
	
	protected R parse(String recordStr, boolean allColumns) throws ParseException, IllegalArgumentException, NullPointerException
	{
		R record = getNewRecord();
		record.parse(recordStr, allColumns ? null : skipColumns);
		return record;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#toString(java.lang.Object)
	 */
	@Override
	protected String toString(R record)
	{
		return toString(record, fullSerialisation);
	}
	
	protected String toString(R record, boolean allColumns)
	{
		return record.serialise(allColumns ? null : skipColumns);
	}

	@Override
	protected void write(R record, BitOutputStream bitStream) throws IOException
	{
		for(Column<?> subCol : schema.getColumns())
			if(!skipColumns.contains(subCol))
				subCol.writeObject(getRecordColumn(subCol).retrieveValue(record), bitStream); // will also write optional bit of the subcolumn if it is optional
	}
	
	protected abstract R getNewRecord();

	@Override
	protected R read(BitInputStream bitStream) throws IOException
	{
		R record = getNewRecord();
		for(Column<?> subCol : schema.getColumns())
			if(!skipColumns.contains(subCol))
				getRecordColumn(subCol).storeObject(record, subCol.readValue(bitStream));
		return record;
	}

	/**
	 * There is nothing to check here, all necessary validation will
	 * happen when data is stored, read or written using the subcolumns.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#validate(java.lang.Object)
	 */
	@Override
	protected void validate(R record) throws IllegalArgumentException
	{
		// does nothing
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
			RecordColumn<R> other = (RecordColumn<R>) otherColumn;
			return	this.schema.equals(other.schema) &&
					CollectionUtils.equals(this.skipColumns, other.skipColumns) &&
					CollectionUtils.equals(this.swapColumns, other.swapColumns);
		}
		return false;
	}

}
