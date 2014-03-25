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

import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.storage.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.storage.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * A composite column, consisting of a set of "subcolumns" as specified by a Schema
 *
 * The "skip columns" mechanism allows certain subcolumns to be skipped when
 * writing/reading records to/from binary storage. Optionally this mechanism can also
 * be applied upon serialisation/deserialisation to/from String, but the default
 * behaviour is to include them.
 * 
 * The "swap columns" mechanism allows to deal with the situation in which we want to use a
 * (slightly) different column for binary storage. Currently only used in {@link LocationColumn}.
 * 
 * @author mstevens
 */
public abstract class RecordColumn<R extends Record> extends Column<R>
{

	static public final char QUALIFIED_NAME_SEPARATOR = '.';
	static public final boolean DEFAULT_FULL_SERIALISATION = true; // don't skip columns upon (de)serialisation by default
	
	private final Schema schema;
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
		if(schema == null)
			throw new NullPointerException("RecordColumn needs a non-null schema to specify its subcolumns.");
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
	 * Specify a column instance to be used instead of one of the schemaColumns
	 * when reading/writing from/to binary storage.
	 * The binaryColumn must be other same content type as the schemaColumn,
	 * but no names, optionality or other other restrictions are checked.
	 * Use with care!
	 * 
	 * @param schemaColumn
	 * @param binaryColumn
	 */
	protected void addBinaryColumn(Column<?> schemaColumn, Column<?> binaryColumn)
	{
		if(!schema.containsColumn(schemaColumn))
			throw new IllegalArgumentException("Unknown subColumn");
		if(!schemaColumn.getType().equals(binaryColumn.getType()))
			throw new IllegalArgumentException("Column type mismatch!");
		swapColumns.put(schemaColumn, binaryColumn);
	}
	
	/**
	 * Get column to use when store/retrieve values in binary form.
	 * This is typically the given schemaColumn itself, unless a "binaryColumn" has been registered for it.
	 * 
	 * @param schemaColumn
	 * @return
	 */
	protected Column<?> getBinaryColumn(Column<?> schemaColumn)
	{
		if(swapColumns.containsKey(schemaColumn))
			return swapColumns.get(schemaColumn);
		else
			return schemaColumn;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#parse(java.lang.String)
	 */
	@Override
	public R parse(String recordStr) throws ParseException, IllegalArgumentException, NullPointerException
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
	public String toString(R record)
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
				getBinaryColumn(subCol).writeObject(subCol.retrieveValue(record), bitStream); // will also write optional bit of the subcolumn if it is optional
	}
	
	/**
	 * @return new "subrecord" instance
	 */
	public abstract R getNewRecord();

	@Override
	protected R read(BitInputStream bitStream) throws IOException
	{
		R record = getNewRecord();
		for(Column<?> subCol : schema.getColumns())
			if(!skipColumns.contains(subCol))
				subCol.storeObject(record, getBinaryColumn(subCol).readValue(bitStream));
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
	
	/**
	 * Default {@link Column#accept(ColumnVisitor)} implementation.
	 * It is recommended that subclasses override this to check whether the visitor doesn't require custom treatment of specific kinds of RecordColumns (e.g. LocationColumn)
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#accept(uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor)
	 */
	@Override
	public void accept(ColumnVisitor visitor)
	{
		accept(visitor, true);
	}

	protected void accept(ColumnVisitor visitor, boolean fullTraverse)
	{
		// Enter record column:
		visitor.enter(this);
		// Traverse subcolumns:
		for(Column<?> subCol : schema.getColumns())
			if(fullTraverse || !skipColumns.contains(subCol))
				subCol.accept(visitor);
		// Leave record column:
		visitor.leave(this);
	}
	
	/**
	 * @return the schema
	 */
	public Schema getSchema()
	{
		return schema;
	}

}
