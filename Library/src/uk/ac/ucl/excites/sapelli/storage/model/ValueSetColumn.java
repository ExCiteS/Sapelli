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
import java.util.Objects;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.shared.io.BitInputStream;
import uk.ac.ucl.excites.sapelli.shared.io.BitOutputStream;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
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
	 * @param defaultValueSet
	 */
	public ValueSetColumn(String name, CS columnSet, boolean optional, VS defaultValueSet)
	{
		this(name, columnSet, optional, defaultValueSet, DEFAULT_INCLUDE_SKIPCOLS_IN_STRING_SERIALISATION, DEFAULT_INCLUDE_VIRTUALCOLS_IN_STRING_SERIALISATION);
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
	 * @param defaultValueSet
	 * @param includeSkipColsInStringSerialisation whether serialisation/deserialisation to/from String should include the subcolumns in skipColumns
	 * @param includeVirtualColsInStringSerialisation whether serialisation/deserialisation to/from String should include virtual subcolumns
	 */
	public ValueSetColumn(String name, CS columnSet, boolean optional, VS defaultValueSet, boolean includeSkipColsInStringSerialisation, boolean includeVirtualColsInStringSerialisation)
	{
		super(name, optional, defaultValueSet);
		if(columnSet == null || !columnSet.isSealed())
			throw new IllegalArgumentException("ValueSetColumn needs a non-null, sealed columnSet to specify its subcolumns.");
		
		// Deal with defaultValueSet:
		if(defaultValueSet != null)
		{
			// Check columnSet:
			if(defaultValueSet.columnSet == columnSet)
				throw new IllegalArgumentException("DefaultValueSet must be of the same columnSet.");
			// Check consistency of defaultValue wrt defaultValues of subcolumns:
			for(Column<?> subCol : columnSet.getColumns(false))
				if(subCol.defaultValue != null && !Objects.deepEquals(subCol.retrieveValue(defaultValueSet), subCol.defaultValue))
					throw new IllegalArgumentException("Default value of subcolumn must be either null or equal to the corresponding value in the defaultValue of the ValueSetColumn");
		}
		
		this.columnSet = columnSet;
		this.includeSkipColsInStringSerialisation = includeSkipColsInStringSerialisation;
		this.includeVirtualColsInStringSerialisation = includeVirtualColsInStringSerialisation;
	}
	
	/**
	 * @return new "sub-ValueSet" instance, in which each value should be set to the {@link #defaultValue} of the corresponding sub-{@link Column}
	 */
	public abstract VS getNewValueSet();
	
	/**
	 * Creates a new "sub-ValueSet" instance and stores it in this column on the given valueSet.
	 * Note that this obviously means the existing sub-ValueSet (i.e. value) in this column will be replaced.
	 * 
	 * @param valueSet
	 * @return the new "sub-ValueSet" instance
	 * @see #getNewValueSet()
	 */
	public VS storeNewValueSet(ValueSet<?> valueSet)
	{
		return storeValue(valueSet, getNewValueSet());
	}
	
	/**
	 * Register a column that needs to be skipped upon reading/writing to binary representations and
	 * optionally also for serialisation/deserialisation to/from String
	 * 
	 * @param schemaColumn a non-virtual(!) column that is part of the schema
	 */
	protected void skipColumn(Column<?> schemaColumn)
	{
		if(schemaColumn == null)
			throw new NullPointerException("skipColumn cannot be null!");
		int position = columnSet.getColumnPosition(schemaColumn.name);
		if(position == Schema.UNKNOWN_COLUMN_POSITION)
			throw new IllegalArgumentException("Unknown subcolumn \"" + schemaColumn.name + "\"!");
		if(skipColumnPositions == null)
			skipColumnPositions = new HashSet<Integer>();
		skipColumnPositions.add(position);
		skipColumns = null; // to ensure the set of Column<?>s is recreated
	}
	
	protected Set<Column<?>> getSkipColumns(boolean forceNone)
	{
		if(skipColumnPositions == null || forceNone)
			return ColumnSet.SKIP_NONE;
		if(skipColumns == null)
		{
			Set<Column<?>> mutableSkipColumns = new HashSet<Column<?>>();
			for(int pos : skipColumnPositions)
				mutableSkipColumns.add(columnSet.getColumn(pos));
			skipColumns = Collections.unmodifiableSet(mutableSkipColumns);
		}
		return skipColumns;
	}
	
	protected boolean isColumnSkipped(Column<?> column)
	{
		return skipColumnPositions != null && skipColumnPositions.contains(columnSet.getColumnPosition(column.name));
	}
	
	/**
	 * Register a column instance, the {@code binaryColumn}, to be used instead of one
	 * of the {@code schemaColumn}s when reading/writing from/to binary representations.
	 * The {@code binaryColumn} must be other same content type as the  {@code schemaColumn},
	 * but no names, optionality or other other restrictions are checked.
	 * Use with care!
	 * 
	 * @param schemaColumn a non-virtual(!) column that is part of the schema
	 * @param binaryColumn a Column to use instead of the  {@code schemaColumn} when reading/writing from/to binary representations
	 */
	protected void swapColumn(Column<?> schemaColumn, Column<?> binaryColumn)
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
	 * Get sub-column to use when reading/writing from/to binary representations.
	 * Often this is given schemaColumn itself, unless it has been swapped for a "binaryColumn".
	 * 
	 * @param schemaColumn
	 * @see #swapColumn(Column, Column)
	 * @return
	 */
	protected Column<?> getBinaryColumn(Column<?> schemaColumn)
	{
		if(swapColumns != null)
		{
			int schemaColPos = columnSet.getColumnPosition(schemaColumn.name);
			if(schemaColPos == Schema.UNKNOWN_COLUMN_POSITION)
				throw new IllegalArgumentException("Unknown subcolumn \"" + schemaColumn.name + "\"!"); // this should never happen
			if(swapColumns.containsKey(schemaColPos))
				return swapColumns.get(schemaColPos);
		}
		return schemaColumn;
	}
	
	/**
	 * @return whether or not all subcolumns are optional
	 */
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
	public VS parse(String valueSetString) throws ParseException, IllegalArgumentException, NullPointerException
	{
		return parse(valueSetString, includeVirtualColsInStringSerialisation, getSkipColumns(includeSkipColsInStringSerialisation));
	}
	
	/**
	 * @param valueSetString the {@link String} to parse, should be neither {@code null} nor empty {@code String} (as those both represent a {@code null} value)
	 * @param includeVirtual
	 * @param skipColumns
	 * @return the parsed ValueSet
	 * @throws ParseException
	 * @throws IllegalArgumentException
	 * @throws NullPointerException
	 * @see #parse(String)
	 * @see {@link ValueSet#serialise(boolean, Set)}
	 */
	public VS parse(String valueSetString, boolean includeVirtual, Set<Column<?>> skipColumns) throws ParseException, IllegalArgumentException, NullPointerException
	{
		if(isApplyingSerialisationDelimiting())
			valueSetString = StringUtils.deescapeByDoublingAndWrapping(valueSetString, getSerialisationDelimiter());
		VS valueSet = getNewValueSet();
		valueSet.parse(valueSetString, includeVirtual, skipColumns); // throws an IllegalArgumentException if the valueSetString is null or ""
		return valueSet;
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#toString(java.lang.Object)
	 */
	@Override
	public String toString(VS valueSet)
	{
		return toString(valueSet, includeVirtualColsInStringSerialisation, getSkipColumns(includeSkipColsInStringSerialisation));
	}
	
	/**
	 * @param valueSet
	 * @param includeVirtual
	 * @param skipColumns
	 * @return
	 * 
	 * @see #toString(ValueSet)
	 * @see {@link ValueSet#serialise(boolean, Set)}
	 */
	public String toString(VS valueSet, boolean includeVirtual, Set<Column<?>> skipColumns)
	{
		String serialisedValueSet = valueSet.serialise(includeVirtual, skipColumns);
		return isApplyingSerialisationDelimiting() ?
			StringUtils.escapeByDoublingAndWrapping(serialisedValueSet, getSerialisationDelimiter(), /*force:*/ true) :
			serialisedValueSet;
	}

	@Override
	protected void write(VS record, BitOutputStream bitStream, boolean lossless) throws IOException
	{
		for(Column<?> subCol : columnSet.getColumns(false))
			if(lossless || !isColumnSkipped(subCol)) // never skip a column if lossless
				getBinaryColumn(subCol).writeObject(subCol.retrieveValue(record), bitStream, lossless); // will also write optional bit of the subcolumn if it is optional
	}
	
	@Override
	protected VS read(BitInputStream bitStream, boolean lossless) throws IOException
	{
		VS valueSet = getNewValueSet();
		for(Column<?> subCol : columnSet.getColumns(false))
			if(lossless || !isColumnSkipped(subCol)) // never skip a column if lossless
				subCol.storeObject(valueSet, getBinaryColumn(subCol).readValue(bitStream, lossless));
		return valueSet;
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#canBeLossy()
	 */
	@Override
	public boolean canBeLossy()
	{
		if(skipColumnPositions != null)
			return true; // some columns may be skipped, when this happens information is most likely lost -> lossy
		for(Column<?> subCol : columnSet.getColumns(false))
			if(getBinaryColumn(subCol).canBeLossy())
				return true; // one of the columns used for binary writing/reading can produce lossy output
		return false;
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
	 * If the value of this column in the given valueSet is currently {@code null} (i.e. the column is "empty") then it will be
	 * reset to the {@link #defaultValue} (assumed be be non-{@code null}), optionally only if the column is required (i.e. non-optional)
	 * Use with care!
	 * 
	 * @param valueSet
	 * @param onlyIfRequired whether to only reset required columns ((@code true}) or also optional ones ({@code false})
	 * @param recurse whether or not to apply the operation recursively to all subColumns
	 *
	 * @see #resetValue(ValueSet)
	 * @see uk.ac.ucl.excites.sapelli.storage.model.Column#resetIfEmpty(uk.ac.ucl.excites.sapelli.storage.model.ValueSet, boolean)
	 */
	@Override
	public final void resetIfEmpty(ValueSet<?> valueSet, boolean onlyIfRequired, boolean recurse) throws IllegalArgumentException
	{
		// At this level:
		super.resetIfEmpty(valueSet, onlyIfRequired, false);
		// Level(s) below:
		VS subValueSet = null;
		if(recurse && (subValueSet = retrieveValue(valueSet)) != null && !subValueSet.hasEqualValues(defaultValue))
			for(Column<?> subCol : columnSet.getColumns(false))
				subCol.resetIfEmpty(subValueSet, onlyIfRequired, true);
	}
	
	/**
	 * Checks, possibly recursively, whether the (sub)value(s) for this column in the given valueSet equals the (sub)column's/s' {@link defaultValue}.
	 *
	 * @param valueSet should not be {@code null}
	 * @return whether or not a non-{@code null} value is set
	 * @throws IllegalArgumentException when this column is not part of the ValueSet's ColumnSet, nor compatible with a column by the same name that is
	 */
	@Override
	public final boolean isValueDefault(ValueSet<?> valueSet) throws IllegalArgumentException
	{
		VS subValueSet = retrieveValue(valueSet);
		if(defaultValue != null)
			return defaultValue.hasEqualValues(subValueSet); // returns false if subValueSet is null
		// defaultValue is null...
		else if(subValueSet == null)
			return true; // subValueSet is too...
		//else: recursive...
		for(Column<?> subCol : columnSet.getColumns(false))
			if(!subCol.isValueDefault(subValueSet))
				return false;
		return true;
	}

	/**
	 * Checks, possibly recursively, whether this column has a valid value in the given valueSet.
	 * A {@code null} value is valid only if it the column is optional. A non-{@code null} value is valid if it passes the {@link #validate(Object)} tests.
	 * 
	 * @param valueSet should not be {@code null}
	 * @param recurse whether or not to check recursively if all subColumns also have valid values
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
				total += getBinaryColumn(subCol).getMaximumSize(lossless);
		return total;
	}

	@Override
	protected int getMinimumValueSize(boolean lossless)
	{
		int total = 0;
		for(Column<?> subCol : columnSet.getColumns(false))
			if(lossless || !isColumnSkipped(subCol))
				total += getBinaryColumn(subCol).getMinimumSize(lossless);
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
