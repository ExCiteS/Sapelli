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

package uk.ac.ucl.excites.sapelli.storage.model;

import java.io.IOException;
import java.text.ParseException;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.storage.model.columns.LocationColumn;
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
	
	static private final long serialVersionUID = 2L;

	static public final char QUALIFIED_NAME_SEPARATOR = '.'; // Note: if this is ever changed Column#SanitiseName(String) may need changing too!
	static public final boolean DEFAULT_INCLUDE_SKIPCOLS_IN_STRING_SERIALISATION = true;
	static public final boolean DEFAULT_INCLUDE_VIRTUALCOLS_IN_BINARY_SERIALISATION = false;
	static public final boolean DEFAULT_INCLUDE_VIRTUALCOLS_IN_STRING_SERIALISATION = true;
	
	private final Schema schema;
	protected final boolean includeSkipColsInStringSerialisation;
	protected final boolean includeVirtualColsInBinarySerialisation;
	protected final boolean includeVirtualColsInStringSerialisation;
	private Set<Column<?>> skipColumns;
	private Map<Column<?>, Column<?>> swapColumns;

	public RecordColumn(Class<R> type, String name, Schema schema, boolean optional)
	{
		this(type, name, schema, optional, DEFAULT_INCLUDE_SKIPCOLS_IN_STRING_SERIALISATION, DEFAULT_INCLUDE_VIRTUALCOLS_IN_BINARY_SERIALISATION, DEFAULT_INCLUDE_VIRTUALCOLS_IN_STRING_SERIALISATION);
	}
	
	/**
	 * @param type
	 * @param name
	 * @param schema schema containing subcolumns
	 * @param optional
	 * @param includeSkipColsInStringSerialisation whether serialisation/deserialisation to/from String should include the subcolumns in skipColumns
	 * @param includeVirtualColsInBinarySerialisation whether writing/reading to/from binary storage should include virtual subcolumns
	 * @param includeVirtualColsInStringSerialisation whether serialisation/deserialisation to/from String should include virtual subcolumns
	 */
	public RecordColumn(Class<R> type, String name, Schema schema, boolean optional, boolean includeSkipColsInStringSerialisation, boolean includeVirtualColsInBinarySerialisation, boolean includeVirtualColsInStringSerialisation)
	{
		super(type, name, optional);
		if(schema == null)
			throw new NullPointerException("RecordColumn needs a non-null schema to specify its subcolumns.");
		this.schema = schema;
		this.includeSkipColsInStringSerialisation = includeSkipColsInStringSerialisation;
		this.includeVirtualColsInBinarySerialisation = includeVirtualColsInBinarySerialisation;
		this.includeVirtualColsInStringSerialisation = includeVirtualColsInStringSerialisation;
	}
	
	/**
	 * Add a column that needs to be skipped upon reading/writing to binary storage and
	 * optionally also for serialisation/deserialisation to/from String
	 * 
	 * @param skipColumn
	 */
	protected void addSkipColumn(Column<?> skipColumn)
	{
		if(!schema.containsColumn(skipColumn, true))
			throw new IllegalArgumentException("Unknown subcolumn");
		if(skipColumns == null)
			skipColumns = new HashSet<Column<?>>();
		skipColumns.add(skipColumn);
	}
	
	protected Set<Column<?>> getSkipColumns(boolean forceNone)
	{
		return skipColumns == null || forceNone ? Collections.<Column<?>>emptySet() : skipColumns;
	}
	
	protected boolean isColumnSkipped(Column<?> column)
	{
		return skipColumns != null && skipColumns.contains(column);
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
		if(!schema.containsColumn(schemaColumn, true))
			throw new IllegalArgumentException("Unknown subColumn");
		if(!schemaColumn.getType().equals(binaryColumn.getType()))
			throw new IllegalArgumentException("Column type mismatch!");
		if(swapColumns == null)
			swapColumns = new HashMap<Column<?>, Column<?>>();
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
		if(swapColumns != null && swapColumns.containsKey(schemaColumn))
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
		return parse(recordStr, includeVirtualColsInStringSerialisation, getSkipColumns(includeSkipColsInStringSerialisation));
	}
	
	public R parse(String recordStr, boolean includeVirtual, Set<Column<?>> skipColumns) throws ParseException, IllegalArgumentException, NullPointerException
	{
		R record = getNewRecord();
		record.parse(recordStr, includeVirtual, skipColumns);
		return record;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#toString(java.lang.Object)
	 */
	@Override
	public String toString(R record)
	{
		return toString(record, includeVirtualColsInStringSerialisation, getSkipColumns(includeSkipColsInStringSerialisation));
	}
	
	public String toString(R record, boolean includeVirtual, Set<Column<?>> skipColumns)
	{
		return record.serialise(includeVirtual, skipColumns);
	}

	@Override
	protected void write(R record, BitOutputStream bitStream) throws IOException
	{
		for(Column<?> subCol : schema.getColumns(includeVirtualColsInBinarySerialisation))
			if(!isColumnSkipped(subCol))
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
		for(Column<?> subCol : schema.getColumns(includeVirtualColsInBinarySerialisation))
			if(!isColumnSkipped(subCol))
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
		return schema.getMaximumSize(includeVirtualColsInBinarySerialisation, getSkipColumns(false));
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#_getMinimumSize()
	 */
	@Override
	protected int _getMinimumSize()
	{
		return schema.getMinimumSize(includeVirtualColsInBinarySerialisation, getSkipColumns(false));
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
					(this.skipColumns == null ? other.skipColumns == null :
												this.skipColumns.equals(other.skipColumns)) &&
					(this.swapColumns == null ? other.swapColumns == null :
												this.swapColumns.equals(other.swapColumns));
		}
		return false;
	}
	
	@Override
    public int hashCode()
	{
		int hash = super.hashCode();
		hash = 31 * hash + schema.hashCode();
		hash = 31 * hash + (skipColumns == null ? 0 : skipColumns.hashCode());
		hash = 31 * hash + (includeSkipColsInStringSerialisation ? 0 : 1);
		hash = 31 * hash + (swapColumns == null ? 0 : swapColumns.hashCode());
		return hash;
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
		schema.accept(visitor, getSkipColumns(fullTraverse));
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
