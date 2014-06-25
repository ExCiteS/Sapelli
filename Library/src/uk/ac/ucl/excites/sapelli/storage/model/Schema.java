package uk.ac.ucl.excites.sapelli.storage.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.util.IntegerRangeMapping;
import uk.ac.ucl.excites.sapelli.storage.util.ModelFullException;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * A Schema holds a set of ordered {@link Column}s
 * 
 * @author mstevens
 */
@SuppressWarnings("rawtypes")
public class Schema implements Serializable
{

	// Statics------------------------------------------------------------
	private static final long serialVersionUID = 2L;
	
	static protected final int UNKNOWN_COLUMN_POSITION = -1;
	
	static public final String COLUMN_AUTO_KEY_NAME = "AutoKey";
	
	/**
	 * Identification of "internal" schemata (not part of a Model)
	 * 
	 * The ordinal position (x) within the enum is made negative (formula: -x - 1) and used as the schema ID.
	 * This allows to differentiate "internal" schemata (& their records) from "external"/"client" schemata (& records).
	 */
	static public enum Internal
	{				/* ordinal	-> schema id */
		ANONYMOUS,	/*	0		->	-1 */
		INDEX,		/*	1		->	-2 */
		LOCATION,	/*	2		->	-3 */
		ORIENTATION	/*	3		->	-4 */
		// more later?
	}
	
	// XML attributes (for record exports):
	static public final String ATTRIBUTE_MODEL_ID = "modelID";
	static public final String ATTRIBUTE_MODEL_SCHEMA_NUMBER = "modelSchemaNumber";
	static public final String ATTRIBUTE_SCHEMA_NAME = "schemaName";
	
	// v1.x-style identification (for backwards compatibility only):
	//	Note: schemaID & schemaVersion are no longer stored in a Schema instance, instead a 1.x Project instance holds them (Project#id = schemaID & Project#schemaVersion = schemaVersion) 
	static public final int V1X_SCHEMA_ID_SIZE = 24; //bits
	static public final int V1X_SCHEMA_VERSION_SIZE = 8; //bits
	static public final IntegerRangeMapping V1X_SCHEMA_VERSION_FIELD = IntegerRangeMapping.ForSize(0, V1X_SCHEMA_VERSION_SIZE);
	static public final int V1X_DEFAULT_SCHEMA_VERSION = 0;
	// 	Note the XML attributes below have inconsistent naming (for everything else we've been using CamelCase instead of dashes), won't fix because no longer used in v2.x
	static public final String V1X_ATTRIBUTE_SCHEMA_ID = "schema-id";
	static public final String V1X_ATTRIBUTE_SCHEMA_VERSION = "schema-version";
	
	// Dynamics-----------------------------------------------------------
	protected final Model model;
	protected final Internal internal;
	protected final String name;
	private boolean sealed = false;
	
	// Columns & indexes:
	private final List<Column> realColumns = new ArrayList<Column>(); // only contains non-virtual ("real") columns
	private final Map<String, Integer> columnNameToPosition = new LinkedHashMap<String, Integer>(); // only contains non-virtual ("real") columns
	private Map<String, VirtualColumn> virtualColumnsByName;
	private PrimaryKey primaryKey;
	private List<Index> indexes; // also includes the primary key
	private transient List<Column> allColumns; // contains non-virtual and virtual columns
	
	/**
	 * Make a schema instance of an internal kind
	 * 
	 * @param internal
	 */
	public Schema(Internal internal)
	{
		this(internal, internal.name());
	}
	
	/**
	 * Make a schema instance of an internal kind
	 * 
	 * @param internal
	 * @param name
	 */
	public Schema(Internal internal, String name)
	{
		if(internal == null)
			throw new NullPointerException("Please specify an non-null Internal");
		this.internal = internal;
		this.name = name;
		// Internal schemata never have a model:
		this.model = null;
	}
	
	/**
	 * Create a new (external/client) schema instance which will be add to the provided {@link Model}.
	 * 
	 * @param model
	 * @param name
	 * @throws ModelFullException 
	 */
	public Schema(Model model, String name) throws ModelFullException
	{
		if(model == null)
			throw new NullPointerException("Please specify an non-null Model");
		this.model = model;
		int no = model.addSchema(this); // add oneself to the model!
		this.name = (name == null || name.isEmpty() ? model.getName() + "_Schema" + no : name);
		// External/client never have an "internal":
		this.internal = null;
	}
	
	/**
	 * @return the model
	 */
	public Model getModel()
	{
		if(isInternal())
			throw new IllegalStateException("Internal schemata do not belong to a model.");
		return model;
	}
	
	/**
	 * @return whether this is an "internal" (true) or "external"/"client" schema instance 
	 */
	public boolean isInternal()
	{
		return internal != null;
	}
	
	/**
	 * Only supported on "external"/"client" schemata (not on "internal" ones)
	 * 
	 * @return the modelID (unsigned 56 bit integer)
	 */
	public long getModelID()
	{
		return getModel().getID();
	}

	/**
	 * Only supported on "external"/"client" schemata (not on "internal" ones)
	 * 
	 * @return the modelSchemaNo (unsigned 4 bit integer)
	 */
	public int getModelSchemaNumber()
	{
		return getModel().getSchemaNumber(this);
	}

	@SuppressWarnings("unchecked")
	public void addColumns(List<Column<?>> columns)
	{
		for(Column c : columns)
			addColumn(c);
	}
	
	public <T> void addColumn(Column<T> column)
	{
		addColumn(column, true);
	}
	
	protected <T> void addColumn(Column<T> column, boolean useVirtual)
	{
		if(sealed)
			throw new IllegalStateException("Cannot extend a sealed schema!");
		if(containsColumn(column, true))
			throw new IllegalArgumentException("The schema already contains a column with name \"" + column.getName() + "\"!");
		// Add the column:
		columnNameToPosition.put(column.getName(), realColumns.size());
		realColumns.add(column);
		// If we are allowed then add any virtual versions it may have:		
		if(useVirtual)
			for(VirtualColumn<?, T> vCol : column.getVirtualVersions())
			{
				if(!containsColumn(vCol.getSourceColumn(), false))
					throw new IllegalArgumentException("The schema does not contain the source column (" + vCol.getSourceColumn().getName() + ") of the given virtual column.");
				if(virtualColumnsByName == null)
					virtualColumnsByName = new HashMap<String, VirtualColumn>();
				virtualColumnsByName.put(vCol.getName(), vCol);
			}
	}
	
	/**
	 * Add an {@link Index} to the Schema. If the Index is a primary key it is added as such (provided does not already have a primary key).
	 * Adding indexes is possible after the Schema has been sealed (setting/changing the primary key is not).
	 * 
	 * @param index
	 */
	public void addIndex(Index index)
	{
		if(index instanceof PrimaryKey)
			// set as primary key:
			setPrimaryKey((PrimaryKey) index);
		else
			// add as normal index:
			doAddIndex(index);
	}
	
	/**
	 * @param primaryKey
	 */
	public void setPrimaryKey(PrimaryKey primaryKey)
	{
		if(this.primaryKey != null)
			throw new IllegalStateException("This Schema already has a primary key (there can be only 1)!");
		if(sealed)
			throw new IllegalStateException("Cannot set primary key on a sealed schema!");
		// Also add as an index (+ do checks):
		doAddIndex(primaryKey);
		// Set primary key:
		this.primaryKey = primaryKey;
	}

	private void doAddIndex(Index index)
	{
		// Null check:
		if(index == null)
			throw new NullPointerException("Index cannot be null!");
		// Check if the indexed columns are columns of this Schema instance:
		for(Column idxCol : index.getColumns(false))
			if(!containsColumn(idxCol, false))
				throw new IllegalArgumentException("Indexed column '" + idxCol.getName() + "' does not belong to this Schema. Indexed columns need to be added to the Schema before indexes are added or the primary key is set.");
		// Initialise collection if needed:
		if(indexes == null)
			indexes = new ArrayList<Index>();
		// Add to the indexes:
		indexes.add(index);
	}
	
	/**
	 * Seals the schema. After this records can be created based on the schema, but no more columns can be added and the primary key cannot be set or changed.
	 * <br/>
	 * If an "external/client" schema has not received a primary key at this point an auto-incrementing integer primary key is added prior to sealing. For internal schemata does not happen.
	 */
	public void seal()
	{
		// Add automatic primary key to "external/client" schemata that don't have one yet:
		if(!isInternal() && !hasPrimaryKey())
		{
			IntegerColumn intColumn = new IntegerColumn(COLUMN_AUTO_KEY_NAME, false, true, Long.SIZE); // signed 64 bit, based on ROWIDs in SQLite v3 and later (http://www.sqlite.org/version3.html)
			setPrimaryKey(new AutoIncrementingPrimaryKey(name + "_Idx" + COLUMN_AUTO_KEY_NAME, intColumn));
		}
		// Seal:
		this.sealed = true;
	}
	
	/**
	 * @return whether or not this schema is sealed
	 */
	public boolean isSealed()
	{
		return sealed;
	}
	
	/**
	 * @param name
	 * @param checkVirtual whether or not to look in the schema's virtual columns
	 * @return the {@link Column} instance with this name, or {@code null} if the Schema contains no such column
	 */
	public Column getColumn(String name, boolean checkVirtual)
	{
		Integer pos = columnNameToPosition.get(name);
		if(pos == null)
			return checkVirtual && virtualColumnsByName != null ? virtualColumnsByName.get(name) : null;
		return realColumns.get(pos);
	}
	
	/**
	 * Returns the non-virtual ("real") column at the given position
	 * 
	 * @param position
	 * @return
	 */
	protected Column<?> getColumn(int position)
	{
		if(position < 0 || position >= realColumns.size())
			throw new ArrayIndexOutOfBoundsException("Invalid column position (" + position + ")");
		return realColumns.get(position);
	}
	
	/**
	 * Returns the position of a given non-virtual column.
	 * Null is returned if the schema doesn't contain the column at all, but also if the column is virtual (even if the source is contained by the schema).
	 * 
	 * @param realColumn a non-virtual column
	 * @return	the position of the given {@link Column} instance within this Schema, or {@link #UNKNOWN_COLUMN_POSITION} if the Schema contains no such column
	 */
	protected int getColumnPosition(Column realColumn)
	{
		Integer idx = columnNameToPosition.get(realColumn.getName());
		if(idx == null)
			return UNKNOWN_COLUMN_POSITION;
		return idx.intValue();
	}

	public List<Column> getColumns(boolean includeVirtual)
	{
		if(includeVirtual && virtualColumnsByName != null) // includeVirtual=true and we have at least 1 virtual column 
		{
			if(!sealed || allColumns == null) // (re)initialise the allColumns list if it is null or as long as the schema is not sealed.
			{
				allColumns = new ArrayList<Column>();
				for(Column<?> nonVirtualCol : realColumns)
				{
					allColumns.add(nonVirtualCol); // "real" column
					// insert virtual versions of real column after it (if they are known by the schema):
					for(VirtualColumn<?, ?> vCol : nonVirtualCol.getVirtualVersions())
						/* check if this vCol was added through addColumn(), because it might have been
						 * added to its "real" parent after the latter had been added to the schema).
						 * We use vCol == getColumn(...) instead of containsColumn(vCol) because we
						 * that would use equals() instead of ==. */
						if(vCol == getColumn(vCol.getName(), true))
							allColumns.add(vCol);
				}
			}
			return allColumns;
		}
		return new ArrayList<Column>(realColumns);
	}
	
	/**
	 * @return an unordered collection of the virtual columns in the schema
	 */
	public Collection<VirtualColumn> getVirtualColumns()
	{
		return virtualColumnsByName == null ? Collections.<VirtualColumn> emptyList() : virtualColumnsByName.values();
	}
	
	/**
	 * @param name the name of a column
	 * @param checkVirtual whether or not to look in the schema's virtual columns
	 * @return
	 */
	public boolean containsColumn(String name, boolean checkVirtual)
	{
		return columnNameToPosition.containsKey(name) || (checkVirtual && virtualColumnsByName != null && virtualColumnsByName.containsKey(name));
	}
	
	/**
	 * 
	 * @param column
	 * @param checkVirtual whether or not to look in the schema's virtual columns
	 * @return	whether or not this Schema contains the given Column or an exact equivalent of it
	 */
	public boolean containsColumn(Column column, boolean checkVirtual)
	{
		Column myColumn = getColumn(column.getName(), checkVirtual);
		return myColumn != null && myColumn.equals(column);
	}
	
	/**
	 * Returns all indexes (if there are any), including the primary key (if one is set).
	 * 
	 * @return the indexes
	 */
	public List<Index> getIndexes()
	{
		return indexes == null ? Collections.<Index> emptyList() : indexes;
	}

	/**
	 * @param includePrimaryKey
	 * @return the indexes
	 */
	public List<Index> getIndexes(boolean includePrimaryKey)
	{
		List<Index> idxs = getIndexes();
		if(!includePrimaryKey)
			idxs.remove(primaryKey);
		return idxs;
	}

	/**
	 * @return the primaryKey
	 */
	public PrimaryKey getPrimaryKey()
	{
		return primaryKey;
	}
	
	/**
	 * @return	whether or not the Schema has a primary key
	 */
	public boolean hasPrimaryKey()
	{
		return primaryKey != null;
	}

	public Record createRecord()
	{
		return new Record(this);
	}
	
	/**
	 * @return the name
	 */
	public String getName()
	{
		return name;
	}
	
	public int getNumberOfColumns(boolean includeVirtual)
	{
		return realColumns.size() + (includeVirtual && virtualColumnsByName != null ? virtualColumnsByName.size() : 0);
	}

	/**
	 * @return whether or not the size taken up by binary stored records of this schema varies at run-time (i.e. depending on data input)
	 */
	public boolean isVariableSize()
	{
		return isVariableSize(false, Collections.<Column<?>>emptySet());
	}
	
	/**
	 * @return whether or not the size taken up by binary stored records of this schema varies at run-time (i.e. depending on data input)
	 * 
	 * @param includeVirtual
	 * @param skipColumns columns to ignore in the total
	 * @return
	 */
	public boolean isVariableSize(boolean includeVirtual, Set<Column<?>> skipColumns)
	{
		for(Column<?> c : getColumns(includeVirtual))
			if(!skipColumns.contains(c) && c.isVariableSize())
				return true;
		return false;
	}
	
	/**
	 * Returns the minimum effective number of bits a record of this schema takes up when written to a binary representation.
	 * Includes all non-virtual columns in the count.
	 * 
	 * @return
	 */
	public int getMinimumSize()
	{
		return getMinimumSize(false, Collections.<Column<?>>emptySet());
	}
	
	/**
	 * Returns the minimum effective number of bits a record of this schema takes up when written to a binary representation.
	 * 
	 * @param includeVirtual
	 * @param skipColumns columns to ignore in the total
	 * @return
	 */
	public int getMinimumSize(boolean includeVirtual, Set<Column<?>> skipColumns)
	{
		int total = 0;
		for(Column<?> c : getColumns(includeVirtual))
			if(!skipColumns.contains(c))
				total += c.getMinimumSize();
		return total;
	}
	
	/**
	 * Returns the maximum effective number of bits a record of this schema takes up when written to a binary representation.
	 * Includes all non-virtual columns in the count.
	 * 
	 * @return
	 */
	public int getMaximumSize()
	{
		return getMaximumSize(false, Collections.<Column<?>>emptySet());
	}

	/**
	 * Returns the maximum effective number of bits a record of this schema takes up when written to a binary representation.
	 * 
	 * @param includeVirtual
	 * @param skipColumns columns to ignore the total
	 * @return
	 */
	public int getMaximumSize(boolean includeVirtual, Set<Column<?>> skipColumns)
	{
		int total = 0;
		for(Column<?> c : getColumns(includeVirtual))
			if(!skipColumns.contains(c))
				total += c.getMaximumSize();
		return total;
	}
	
	/**
	 * Check for equality
	 * 
	 * @param obj object to compare this one with
	 * @return whether or not the given Object is a Schema with the same ID & version as this one
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj)
	{
		return equals(obj, true, true, true);
	}

	/**
	 * Check if the provided object is an identical/equivalent Schema. The usageID & usageSubID are always checked, names and columns are optionally checked, descriptions are ignored. 
	 * 
	 * @param obj object to compare this one with
	 * @param checkNames whether or not to compare the names of the schemas and (if checkColumns is true) those of their columns
	 * @param checkColumns whether or not to compare columns (types, sizes, etc., and names if checkNames is true)
	 * @param checkIndexes whether or not to compare indexes
	 * @return whether or not the given Object is an identical/equivalent Schema (under the given checking conditions)
	 */
	public boolean equals(Object obj, boolean checkNames, boolean checkColumns, boolean checkIndexes)
	{
		if(this == obj) // compare pointers first
			return true;
		if(obj instanceof Schema)
		{
			Schema other = (Schema) obj;
			// Model/Internal
			boolean idMatch = isInternal() ?	(other.isInternal() && this.internal == other.internal) :
												(!other.isInternal() && this.model.getID() == other.model.getID());
			if(!idMatch || !(checkNames || checkColumns || checkIndexes))
				return idMatch;
			// Name:
			if(checkNames && !this.name.equals(other.name))
				return false;
			// Columns & indexes:
			if(checkColumns)
			{
				// Check number of (real) columns:
				if(realColumns.size() != other.realColumns.size())
					return false;
				// Compare columns:
				Iterator<Column> myCols = realColumns.iterator();
				Iterator<Column> otherCols = other.realColumns.iterator();
				while(myCols.hasNext() /* && otherCols.hasNext() */)
					if(!myCols.next().equals(otherCols.next(), checkNames, true))
						return false;
			}
			if(checkIndexes)
			{
				// Check number of indexes:
				if(getIndexes().size() != other.getIndexes().size())
					return false;
				// Compare indexes:
				Iterator<Index> myIndexes = getIndexes().iterator();
				Iterator<Index> otherIndexes = other.getIndexes().iterator();
				while(myIndexes.hasNext() /** otherIndexes.hasNext() */)
					if(!myIndexes.next().equals(otherIndexes.next(), checkNames, checkColumns, false))
						return false;
			}
			return true;
		}
		else
			return false;
	}
	
	@Override
    public int hashCode()
	{
		int hash = 1;
		hash = 31 * hash + (model == null ? 0 : (int)(model.getID() ^ (model.getID() >>> 32))); // do not use model.hashCode() here!
		hash = 31 * hash + (internal == null ? 0 : internal.ordinal());
		hash = 31 * hash + (name == null ? 0 : name.hashCode());
		hash = 31 * hash + realColumns.hashCode();
		hash = 31 * hash + getIndexes().hashCode();
		hash = 31 * hash + (primaryKey == null ? 0 : primaryKey.hashCode());
		hash = 31 * hash + (sealed ? 0 : 1);
		return hash;
	}
	
	@Override
	public String toString()
	{
		return "Schema " + name;
	}
	
	public String getSpecification()
	{
		StringBuffer bff = new StringBuffer();
		bff.append(toString() + ":");
		for(Column<?> c : getColumns(true))
			bff.append("\n\t- " + c.getSpecification());
		// TODO add indexes & primary key to schema specs
		return bff.toString();
	}
	
	public void accept(ColumnVisitor visitor)
	{
		accept(visitor, Collections.<Column<?>>emptySet());
	}
	
	public void accept(ColumnVisitor visitor, Set<Column<?>> skipColumns)
	{
		for(Column<?> c : getColumns(visitor.includeVirtualColumns()))
			if(!skipColumns.contains(c))
				c.accept(visitor);
	}

}
