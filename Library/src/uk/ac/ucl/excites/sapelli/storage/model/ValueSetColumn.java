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
import uk.ac.ucl.excites.sapelli.storage.types.LocationColumn;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * A composite column, consisting of a set of "subcolumns" as specified by a ColumnSet
 *
 * The "skip columns" mechanism allows certain subcolumns to be skipped when
 * writing/reading records to/from binary storage. Optionally this mechanism can also
 * be applied upon serialisation/deserialisation to/from String, but the default
 * behaviour is to include them.
 * 
 * The "swap columns" mechanism allows to deal with the situation in which we want to use a
 * (slightly) different column for binary storage. Currently only used in {@link LocationColumn}.
 * 
 *
 * @param <VS> the ValueSet type (= type of the Column)
 * @param <CS> the ColumnSet type of the ValueSet type
 */
public abstract class ValueSetColumn<VS extends ValueSet<CS>, CS extends ColumnSet> extends Column<VS>
{
	
	static private final long serialVersionUID = 2L;

	static public final char QUALIFIED_NAME_SEPARATOR = '.'; // Note: if this is ever changed Column#SanitiseName(String) may need changing too!
	static public final boolean DEFAULT_INCLUDE_SKIPCOLS_IN_STRING_SERIALISATION = true;
	static public final boolean DEFAULT_INCLUDE_VIRTUALCOLS_IN_STRING_SERIALISATION = true;
	
	private final CS columnSet;
	protected final boolean includeSkipColsInStringSerialisation;
	protected final boolean includeVirtualColsInStringSerialisation;
	private Set<Integer> skipColumnPositions;
	private transient Set<Column<?>> skipColumns;
	private Map<Integer, Column<?>> swapColumns;

	/**
	 * @param name
	 * @param columnSet
	 * @param optional
	 */
	public ValueSetColumn(String name, CS columnSet, boolean optional)
	{
		this(name, columnSet, optional, null);
	}
	
	/**
	 * @param name
	 * @param columnSet
	 * @param optional
	 * @param defaultValue
	 */
	public ValueSetColumn(String name, CS columnSet, boolean optional, VS defaultValue)
	{
		this(name, columnSet, optional, defaultValue, DEFAULT_INCLUDE_SKIPCOLS_IN_STRING_SERIALISATION, DEFAULT_INCLUDE_VIRTUALCOLS_IN_STRING_SERIALISATION);
	}
	
	/**
	 * @param name
	 * @param schema schema containing subcolumns
	 * @param optional
	 * @param includeSkipColsInStringSerialisation whether serialisation/deserialisation to/from String should include the subcolumns in skipColumns
	 * @param includeVirtualColsInStringSerialisation whether serialisation/deserialisation to/from String should include virtual subcolumns
	 */
	public ValueSetColumn(String name, CS columnSet, boolean optional, boolean includeSkipColsInStringSerialisation, boolean includeVirtualColsInStringSerialisation)
	{
		this(name, columnSet, optional, null, includeSkipColsInStringSerialisation, includeVirtualColsInStringSerialisation);
	}
	
	/**
	 * @param name
	 * @param columnSet
	 * @param optional
	 * @param defaultValue
	 * @param includeSkipColsInStringSerialisation whether serialisation/deserialisation to/from String should include the subcolumns in skipColumns
	 * @param includeVirtualColsInStringSerialisation whether serialisation/deserialisation to/from String should include virtual subcolumns
	 */
	public ValueSetColumn(String name, CS columnSet, boolean optional, VS defaultValue, boolean includeSkipColsInStringSerialisation, boolean includeVirtualColsInStringSerialisation)
	{
		super(name, optional, defaultValue);
		if(columnSet == null || !columnSet.isSealed())
			throw new IllegalArgumentException("RecordColumn needs a non-null, sealed columnSet to specify its subcolumns.");
		this.columnSet = columnSet;
		this.includeSkipColsInStringSerialisation = includeSkipColsInStringSerialisation;
		this.includeVirtualColsInStringSerialisation = includeVirtualColsInStringSerialisation;
	}
	
	/**
	 * Add a column that needs to be skipped upon reading/writing to binary storage/transmission and
	 * optionally also for serialisation/deserialisation to/from String
	 * 
	 * @param skipColumn a non-virtual(!) column that is part of the schema
	 */
	protected void addSkipColumn(Column<?> skipColumn)
	{
		if(skipColumn == null)
			throw new NullPointerException("skipColumn cannot be null!");
		int position = columnSet.getColumnPosition(skipColumn.name);
		if(position == Schema.UNKNOWN_COLUMN_POSITION)
			throw new IllegalArgumentException("Unknown subcolumn \"" + skipColumn.name + "\"!");
		if(skipColumnPositions == null)
			skipColumnPositions = new HashSet<Integer>();
		skipColumnPositions.add(position);
		skipColumns = null; // to ensure the set of Column<?>s is recreated
	}
	
	protected Set<Column<?>> getSkipColumns(boolean forceNone)
	{
		if(skipColumnPositions == null || forceNone)
			return Collections.<Column<?>> emptySet();
		if(skipColumns == null)
		{
			skipColumns = new HashSet<Column<?>>();
			for(int pos : skipColumnPositions)
				skipColumns.add(columnSet.getColumn(pos));
		}
		return skipColumns;
	}
	
	protected boolean isColumnSkipped(Column<?> column)
	{
		return skipColumnPositions != null && skipColumnPositions.contains(columnSet.getColumnPosition(column.name));
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
		if(schemaColumn == null)
			throw new NullPointerException("schemaColumn cannot be null!");
		if(binaryColumn == null)
			throw new NullPointerException("binaryColumn cannot be null!");
		int schemaColPos = columnSet.getColumnPosition(schemaColumn.name);
		if(schemaColPos == Schema.UNKNOWN_COLUMN_POSITION)
			throw new IllegalArgumentException("Unknown subcolumn \"" + schemaColumn.name + "\"!");
		if(!schemaColumn.getType().equals(binaryColumn.getType()))
			throw new IllegalArgumentException("Column type mismatch!");
		if(swapColumns == null)
			swapColumns = new HashMap<Integer, Column<?>>();
		swapColumns.put(schemaColPos, binaryColumn);
	}
	
	/**
	 * Get column to use when store/retrieve values in binary form.
	 * This is typically the given schemaColumn itself, unless a "binaryColumn" has been registered for it.
	 * 
	 * @param schemaColumn
	 * @param forceLossless if {@code true} the "binaryColumn" will only be returned if it is lossless, otherwise the given (assumed lossless) schemaColumn is returned
	 * @return
	 */
	protected Column<?> getBinaryColumn(Column<?> schemaColumn, boolean forceLossless)
	{
		if(!forceLossless && swapColumns != null)
		{
			int schemaColPos = columnSet.getColumnPosition(schemaColumn.name);
			if(schemaColPos == Schema.UNKNOWN_COLUMN_POSITION)
				throw new IllegalArgumentException("Unknown subcolumn \"" + schemaColumn.name + "\"!"); // this should never happen
			if(swapColumns.containsKey(schemaColPos))
			{
				Column<?> binaryCol = swapColumns.get(schemaColPos);
				if(!forceLossless || !binaryCol.canBeLossy())
					return binaryCol;
			}
		}
		return schemaColumn;
	}
	
	public boolean hasAllOptionalSubColumns()
	{
		for(Column<?> subCol : columnSet.getColumns(false))
			if(!subCol.optional)
				return false;
		return true;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#parse(java.lang.String)
	 */
	@Override
	public VS parse(String recordStr) throws ParseException, IllegalArgumentException, NullPointerException
	{
		return parse(recordStr, includeVirtualColsInStringSerialisation, getSkipColumns(includeSkipColsInStringSerialisation));
	}
	
	public VS parse(String recordStr, boolean includeVirtual, Set<Column<?>> skipColumns) throws ParseException, IllegalArgumentException, NullPointerException
	{
		VS record = getNewValueSet();
		record.parse(recordStr, includeVirtual, skipColumns);
		return record;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#toString(java.lang.Object)
	 */
	@Override
	public String toString(VS record)
	{
		return toString(record, includeVirtualColsInStringSerialisation, getSkipColumns(includeSkipColsInStringSerialisation));
	}
	
	public String toString(VS record, boolean includeVirtual, Set<Column<?>> skipColumns)
	{
		return record.serialise(includeVirtual, skipColumns);
	}

	@Override
	protected void write(VS record, BitOutputStream bitStream, boolean lossless) throws IOException
	{
		for(Column<?> subCol : columnSet.getColumns(false))
			if(lossless || !isColumnSkipped(subCol))
				getBinaryColumn(subCol, lossless).writeObject(subCol.retrieveValue(record), bitStream, lossless); // will also write optional bit of the subcolumn if it is optional
	}
	
	/**
	 * @return new "subrecord" instance
	 */
	public abstract VS getNewValueSet();

	@Override
	protected VS read(BitInputStream bitStream, boolean lossless) throws IOException
	{
		VS valueSet = getNewValueSet();
		for(Column<?> subCol : columnSet.getColumns(false))
			if(lossless || !isColumnSkipped(subCol))
				subCol.storeObject(valueSet, getBinaryColumn(subCol, lossless).readValue(bitStream, lossless));
		return valueSet;
	}

	/**
	 * Checks, possibly recursively, whether a non-{code null} value for this column is set in the given valueSet.
	 *
	 * @param valueSet should not be {@code null}
	 * @param recurse whether or not to recursively check if all subColumns also have a non-{@code null} value
	 * @return whether or not a non-{@code null} value is set for this column (if recursive is {@code false}), and also all its sub[...]columns (if recurse if {@code true})
	 * @throws IllegalArgumentException when this column is not part of the record's schema, nor compatible with a column by the same name that is
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#isValuePresent(uk.ac.ucl.excites.sapelli.storage.model.ValueSet, boolean)
	 */
	@Override
	public boolean isValuePresent(ValueSet<?> valueSet, boolean recurse) throws IllegalArgumentException
	{
		VS subValueSet = retrieveValue(valueSet);
		if(subValueSet == null || !recurse)
			return subValueSet != null;
		//else: recursive...
		for(Column<?> subCol : columnSet.getColumns(false))
			if(!subCol.isValuePresent(subValueSet, recurse))
				return false;
		return true;
	}
	
	/**
	 * Checks, possibly recursively, whether this column either has a non-{code null} value in the given valueSet or is optional.
	 * 
	 * @param valueSet should not be {@code null}
	 * @param recurse whether or not to check recursively if all subColumns also have a non-{@code null} value or are optional
	 * @return whether a non-{@code null} value is set or the column is optional (if recursive is {@code false}), and whether the same applies for all its sub[...]columns (if recurse if {@code true})
	 * @throws IllegalArgumentException when this column is not part of the record's schema, nor compatible with a column by the same name that is
	 */
	@Override
	public final boolean isValuePresentOrOptional(ValueSet<?> valueSet, boolean recurse) throws IllegalArgumentException
	{
		VS subValueSet = retrieveValue(valueSet);
		if(subValueSet == null || !recurse)
			return subValueSet != null || optional;
		//else: recursive...
		for(Column<?> subCol : columnSet.getColumns(false))
			if(!subCol.isValuePresentOrOptional(subValueSet, recurse))
				return false;
		return true;
	}
	
	/**
	 * Checks, possibly recursively, whether this column has a valid value in the given valueSet.
	 * A {@code null} value is valid only if it the column is optional. A non-{@code null} value is valid if it passes the {@link #validate(Object)} tests.
	 * 
	 * @param valueSet should not be {@code null}
	 * @param recurse whether or not to check recursively if all subColumns also valid values
	 * @return whether the value currently contained by the valueSet for this column is valid (if recursive is {@code false}), and whether the same applies for all its sub[...]columns (if recurse if {@code true})
	 * @throws IllegalArgumentException when this column is not part of the ValueSet's ColumnSet, nor compatible with a column by the same name that is
	 */
	@Override
	public boolean isValueValid(ValueSet<?> valueSet, boolean recurse) throws IllegalArgumentException
	{
		VS subValueSet = retrieveValue(valueSet);
		if(subValueSet == null || !recurse)
			return isValidValue(subValueSet);
		//else: recursive...
		for(Column<?> subCol : columnSet.getColumns(false))
			if(!subCol.isValueValid(subValueSet, recurse))
				return false;
		return true;
	}

	/**
	 * Default implementation does not check anything, assuming all necessary validation will
	 * happen when data is stored, read or written using the subcolumns.
	 * 
	 * May be overridden.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#validate(java.lang.Object)
	 */
	@Override
	protected void validate(VS record) throws IllegalArgumentException
	{
		// does nothing
	}

	@Override
	protected int getMaximumValueSize(boolean lossless)
	{
		int total = 0;
		for(Column<?> subCol : columnSet.getColumns(false))
			if(lossless || !isColumnSkipped(subCol))
				total += getBinaryColumn(subCol, lossless).getMaximumSize(lossless);
		return total;
	}

	@Override
	protected int getMinimumValueSize(boolean lossless)
	{
		int total = 0;
		for(Column<?> subCol : columnSet.getColumns(false))
			if(lossless || !isColumnSkipped(subCol))
				total += getBinaryColumn(subCol, lossless).getMinimumSize(lossless);
		return total;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#isRequired(boolean)
	 */
	@Override
	public boolean isRequired(boolean recurse)
	{
		if(optional || !recurse)
			return !optional; 
		for(Column<?> subCol : columnSet.getColumns(false))
			if(!subCol.isRequired(recurse))
				return false;
		return true;
	}

	/**
	 * @param subColumn
	 * @return
	 * @throws IllegalArgumentException when the given column is not a subcolumn of this ValueSetColumn
	 */
	public String getQualifiedSubColumnName(Column<?> subColumn) throws IllegalArgumentException
	{
		return new ColumnPointer<Column<?>>(Collections.singletonList(this), subColumn).getQualifiedColumnName(QUALIFIED_NAME_SEPARATOR);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#equalRestrictions(uk.ac.ucl.excites.sapelli.storage.model.Column)
	 */
	@Override
	protected boolean equalRestrictions(Column<VS> otherColumn)
	{
		if(otherColumn instanceof ValueSetColumn)
		{
			ValueSetColumn<?, ?> other = (ValueSetColumn<?, ?>) otherColumn;
			return	this.columnSet.equals(other.columnSet) &&
					(this.skipColumnPositions == null ? other.skipColumnPositions == null :
												this.skipColumnPositions.equals(other.skipColumnPositions)) &&
					this.includeSkipColsInStringSerialisation == other.includeSkipColsInStringSerialisation &&
					this.includeVirtualColsInStringSerialisation == other.includeVirtualColsInStringSerialisation &&
					(this.swapColumns == null ? other.swapColumns == null :
												this.swapColumns.equals(other.swapColumns));
		}
		return false;
	}
	
	@Override
	public int hashCode()
	{
		int hash = super.hashCode();
		hash = 31 * hash + columnSet.hashCode();
		hash = 31 * hash + (skipColumnPositions == null ? 0 : skipColumnPositions.hashCode());
		hash = 31 * hash + (includeSkipColsInStringSerialisation ? 0 : 1);
		hash = 31 * hash + (includeVirtualColsInStringSerialisation ? 0 : 1);
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
		// Enter valueset column:
		visitor.enter(this);
		// Traverse subcolumns:
		columnSet.accept(visitor, getSkipColumns(fullTraverse));
		// Leave valueset column:
		visitor.leave(this);
	}
	
	/**
	 * @return the columnSet
	 */
	public CS getColumnSet()
	{
		return columnSet;
	}

}
