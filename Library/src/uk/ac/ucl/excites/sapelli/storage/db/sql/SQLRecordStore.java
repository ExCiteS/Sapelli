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

package uk.ac.ucl.excites.sapelli.storage.db.sql;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.shared.db.DBException;
import uk.ac.ucl.excites.sapelli.shared.util.TransactionalStringBuilder;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Index;
import uk.ac.ucl.excites.sapelli.storage.model.ListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.RecordColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.LineColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.LocationColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.OrientationColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.PolygonColumn;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.AndConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.ConstraintVisitor;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.EqualityConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.NotConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.OrConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.RuleConstraint;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * @author mstevens
 *
 * @param <SRS>
 * @param <STable>
 * @param <SColumn>
 */
public abstract class SQLRecordStore<SRS extends SQLRecordStore<SRS, STable, SColumn>, STable extends SQLRecordStore<SRS, STable, SColumn>.SQLTable, SColumn extends SQLRecordStore<SRS, STable, SColumn>.SQLColumn<?, ?>> extends RecordStore
{
	
	static protected final String SCHEMATA_TABLE_NAME = "Schemata";
	static protected final String SPACE = " ";	
	
	private Map<Schema, STable> tables;
	private STable schemataTable;

	/**
	 * @param client
	 * @param tableSpecFactory
	 * @throws DBException 
	 */
	public SQLRecordStore(StorageClient client) throws DBException
	{
		super(client);
		this.tables = new HashMap<Schema, STable>();
	}
	
	/**
	 * Must be called from subclass constructor!
	 * 
	 * @param newDB whether or not the database file is new (i.e. empty)
	 * @throws Exception
	 */
	protected void initialise(boolean newDB) throws Exception
	{
		this.schemataTable = getTable(Schema.META_SCHEMA, newDB); // creates the Schemata table if it doesn't exist yet (i.e. for a new database)
	}
	
	protected abstract void executeSQL(String sql) throws DBException;
	
	protected abstract String sanitiseIdentifier(String identifier);
	
	/**
	 * @return whether or not the DBMS supports concurrent connections
	 */
	protected abstract boolean supportsConcurrentConnections();
	
	/**
	 * Checks whether a table with the given name exists in the database.
	 * 
	 * @param tableName
	 * @return
	 */
	protected abstract boolean doesTableExist(String tableName);

	/**
	 * 
	 * @param schema
	 * @param createWhenNotInDB
	 * @return
	 * @throws DBException 
	 */
	protected STable getTable(Schema schema, boolean createWhenNotInDB) throws DBException
	{
		// Check cache of known tables:
		STable table =
			(schema == Schema.META_SCHEMA ?
				schemataTable /* may still be null if getTable() was called from initialise() */ :
				tables.get(schema));
		
		// If not found, generate new SQLTable object for the Schema:
		if(table == null)
		{
			table = getTableFactory().generateTable(schema);
			if(schema != Schema.META_SCHEMA) // the "tables" map is only for tables of "real" schemata!
				tables.put(schema, table);
		}
		
		// If requested then create the actual table in the database if it is not there:
		if(createWhenNotInDB && !table.isInDB())
			createAndRegisterTable(table);
		
		return table;
	}
	
	/**
	 * @param table
	 * @throws DBException
	 */
	private void createAndRegisterTable(STable table) throws DBException
	{		
		startTransaction();
		try
		{
			// Create the table itself:
			table.create();

			// Register the schema (which the table corresponds to) in the schemataTable; unless the table it is the schemataTable itself:
			if(table.schema != Schema.META_SCHEMA)
				schemataTable.insert(Schema.GetMetaRecord(table.schema));
		}
		catch(Exception e)
		{
			rollbackTransactions();
			throw new DBException("Exception upon creating and registering " + table.toString(), e);
		}
		commitTransaction();
	}
	
	/**
	 * TODO use this upon close on empty tables?
	 * 
	 * @param table
	 * @throws DBException
	 */
	private void dropAndForgetTable(STable table) throws DBException
	{
		if(table == schemataTable)
			throw new IllegalArgumentException("Cannot drop the " + SCHEMATA_TABLE_NAME + " table");
		startTransaction();
		try
		{
			// Unregister (i.e. forget) the schema (which the table corresponds to) in the schemataTable:
			schemataTable.delete(Schema.GetMetaRecord(table.schema));
			
			// Drop the table itself:
			table.drop();
		}
		catch(Exception e)
		{
			rollbackTransactions();
			throw new DBException("Exception upon creating and registering " + table.toString(), e);
		}
		commitTransaction();
	}
	
	protected Set<Schema> getAllKnownSchemata()
	{
		// TODO query for all schemata in schemataTable in DB !!!!
		
//		for(Record metaSchemaRec : retrieveRecords(Schema.META_SCHEMA))
//		{
//			Schema schema = Schema.FromMetaRecord(metaSchemaRec);
//			
//			
//			tables.put(schema, getTableFactory().generateTable(schema));
//		}
		
		return tables.keySet();
	}
	
	protected abstract TableFactory getTableFactory();
	
	/**
	 * Default implementation for SQL databases: first do a SELECT based on the key to verify whether the
	 * record exists, then do either UPDATE or INSERT.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#doStore(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	protected boolean doStore(Record record) throws DBException
	{
		boolean newRec = retrieveRecord(record.getReference().getRecordQuery()) == null;
		STable table = getTable(record.getSchema(), true);
		if(newRec)
			table.insert(record);
		else
			table.update(record);
		return newRec;
	}

	@Override
	protected void doDelete(Record record) throws DBException
	{
		STable table = getTable(record.getSchema(), false); // no need to create the table in the db if it isn't there!
		if(table.isInDB()) // !!!
			table.delete(record);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#retrieveRecords(uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery)
	 */
	@Override
	public List<Record> retrieveRecords(RecordsQuery query)
	{
		List<Record> result = new ArrayList<Record>();
		// Run subqueries for each schema in the query, or all known schemata (if the query for "any" schema):
		for(Schema s : query.isAnySchema() ? getAllKnownSchemata() : query.getSourceSchemata())
			try
			{
				STable table = getTable(s, false); // won't throw DBException because we don't allow table creation (so we ignore exception below)
				if(table.isInDB())
					queryForRecords(table, query, result);
			}
			catch(DBException ignore) {}
		return result;
	}
	
	/**
	 * @param schema
	 * @param query
	 * @param result
	 * @throws DBException 
	 */
	protected abstract void queryForRecords(STable table, RecordsQuery query, List<Record> result);

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#retrieveRecord(uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery)
	 */
	@Override
	public Record retrieveRecord(SingleRecordQuery query)
	{
		// TODO Now the reduction happens in Java, we should let the database do the work instead! I.e. generate appropriate SQL to do selection & reduction in 1 query
		
		// Run the RecordsQuery:
		List<Record> records = retrieveRecords(query.getRecordsQuery());
		// Execute the SingleRecordQuery (reducing the list to 1 record), without re-running the recordsQuery, and then return the result:
		return query.execute(records, false);
	}
	
	/**
	 * Get subclasses may need to override this because some SQL dialects use != instead of <> (Note: SQLite supports both)
	 * 
	 * @param comparison
	 * @return
	 */
	public String getComparisonOperator(RuleConstraint.Comparison comparison)
	{
		switch(comparison)
		{
			case SMALLER : return "<";
			case SMALLER_OR_EQUAL : return "<=";
			case EQUAL : return "=";
			case NOT_EQUAL : return "<>";
			case GREATER_OR_EQUAL : return ">=";
			case GREATER : return ">";
		}
		return null; // this should never happen
	}
	
	/**
	 * @author mstevens
	 *
	 */
	public abstract class SQLTable
	{

		public final String tableName;
		public final Schema schema;
		private List<String> tableConstraints = Collections.<String> emptyList();
		private List<Index> explicitIndexes;
		private Boolean existsInDB;
		
		/**
		 * Mapping of Sapelli ColumnPointers (usually leaf columns) to corresponding  SQLColumns.
		 */
		public final Map<ColumnPointer, SColumn> sqlColumns;
		
		/**
		 * Mapping of composite Sapelli columns to a list of SQLColumns which they correspond to.
		 * E.g. Location --> (Lat, Lon, ...) 
		 */
		public final Map<RecordColumn<?>, List<SColumn>> composite2SqlColumns;
		
		public SQLTable(Schema schema)
		{
			this.tableName = sanitiseIdentifier(schema == Schema.META_SCHEMA ? SCHEMATA_TABLE_NAME : "Table_" + schema.getModelID() + '_' + schema.getModelSchemaNumber());
			this.schema = schema;
			// Init collections:
			sqlColumns = new LinkedHashMap<ColumnPointer, SColumn>();
			composite2SqlColumns = new HashMap<RecordColumn<?>, List<SColumn>>();
		}
		
		public void addColumn(SColumn sqlColumn)
		{
			ColumnPointer sourceCP = sqlColumn.sourceColumnPointer;
			sqlColumns.put(sourceCP, sqlColumn);
			// Deal with composites...
			while(sourceCP.isSubColumn())
			{
				ColumnPointer parentCP = sourceCP.getParentPointer();
				RecordColumn<?> parentCol = (RecordColumn<?>) parentCP.getColumn();
				List<SColumn> subSQLCols;
				if(composite2SqlColumns.containsKey(parentCol))
					subSQLCols = composite2SqlColumns.get(parentCol);
				else
				{
					subSQLCols = new ArrayList<SColumn>();
					composite2SqlColumns.put(parentCol, subSQLCols);
				}
				subSQLCols.add(sqlColumn);
				// Next parent...
				sourceCP = parentCP;
			}
		}
		
		public void setTableConstraint(List<String> tableConstraints)
		{
			this.tableConstraints = new ArrayList<String>(tableConstraints);
		}
		
		public void setExplicitIndexes(List<Index> indexes)
		{
			this.explicitIndexes = indexes;
		}
		
		public SColumn getSQLColumn(ColumnPointer columnPointer)
		{
			return sqlColumns.get(columnPointer);
		}
		
		public List<SColumn> getSQLColumns(RecordColumn<?> compositeColumn)
		{
			return composite2SqlColumns.get(compositeColumn);
		}
		
		public boolean isInDB()
		{
			if(existsInDB == null)
				existsInDB = doesTableExist(tableName);
			return existsInDB;
		}
		
		/**
		 * Create table in database, as well as any explicit indexes on its columns.
		 * Default implementation, may be overridden by subclasses
		 * 
		 * @throws DBException
		 */
		public void create() throws DBException
		{
			executeSQL(generateCreateTableStatement());
			existsInDB = true; // !!!
			// Create explicit indexes:
			for(Index idx : explicitIndexes)
				executeSQL(generateCreateIndexStatement(idx));
		}
		
		/**
		 * @return sql statement to create database table
		 * 
		 * @see http://www.w3schools.com/sql/sql_create_table.asp
		 * @see http://www.sqlite.org/lang_createtable.html
		 */
		protected String generateCreateTableStatement()
		{
			TransactionalStringBuilder bldr = new TransactionalStringBuilder(SPACE);
			bldr.append("CREATE TABLE");
			// "IF NOT EXISTS"? (probably SQLite specific)
			bldr.append(tableName);
			bldr.append("(");
			bldr.openTransaction(", ");
			// Columns:
			for(SQLColumn<?, ?> sqlCol : sqlColumns.values())
			{
				bldr.openTransaction(SPACE);
				bldr.append(sqlCol.name);
				bldr.append(sqlCol.type);
				bldr.append(sqlCol.constraint);
				bldr.commitTransaction();
			}
			// Table constraints:
			for(String tConstr : tableConstraints)
				bldr.append(tConstr);
			bldr.commitTransaction(false);
			bldr.append(");", false);
			return bldr.toString();
		}
		
		/**
		 * @param idx
		 * @return sql statement to create database table index
		 * 
		 * @see http://www.w3schools.com/sql/sql_create_index.asp
		 * @see http://www.sqlite.org/lang_createindex.html
		 */
		protected String generateCreateIndexStatement(Index idx)
		{
			TransactionalStringBuilder bldr = new TransactionalStringBuilder(SPACE);
			bldr.append("CREATE");
			if(idx.isUnique())
				bldr.append("UNIQUE");
			bldr.append("INDEX");
			// "IF NOT EXISTS"? (probably SQLite specific)
			bldr.append(sanitiseIdentifier(idx.getName()));
			bldr.append("ON");
			bldr.append(tableName);
			bldr.append("(");
			bldr.openTransaction(", ");
			// List indexed columns:
			for(Column<?> idxCol : idx.getColumns(false))
				bldr.append(getSQLColumn(new ColumnPointer(schema, idxCol)).name);
			bldr.commitTransaction(false);
			bldr.append(");", false);
			return bldr.toString();
		}
		
		/**
		 * Drop the table from the database.
		 * Default implementation, can be overridden by subclasses
		 * 
		 * @throws DBException
		 */
		public void drop() throws DBException
		{
			executeSQL(generateDropTableStatement());
			existsInDB = false; // !!!
		}
		
		protected String generateDropTableStatement()
		{
			TransactionalStringBuilder bldr = new TransactionalStringBuilder(SPACE);
			bldr.append("DROP TABLE");
			bldr.append(tableName);
			bldr.append(");", false);
			return bldr.toString();
		}
		
		/**
		 * Insert new record in database
		 * 
		 * May be overridden
		 * 
		 * @param record
		 * @throws DBException
		 */
		public void insert(Record record) throws DBException
		{
			if(!isInDB())
				throw new DBException("Insert failed: " + toString() + " does not exist in database!");
			List<String> values = new ArrayList<String>();
			// Get literal values (quoted if needed):
			for(SQLColumn<?, ?> sqlCol : sqlColumns.values())
				values.add(sqlCol.retrieveAsLiteral(record, true));
			executeSQL(generateInsertStatement(values));
		}
		
		protected String generateInsertStatement(List<String> values)
		{
			if(values.size() != sqlColumns.size())
				throw new IllegalArgumentException("Invalid number of values");
			return generateInsertStatement(values, null);
		}
		
		protected String generateInsertStatement(String valuePlaceHolder)
		{
			return generateInsertStatement(null, valuePlaceHolder);
		}
		
		/**
		 * @param values - if null valuePlaceHolder should not be
		 * @param valuePlaceHolder - if null values should not be
		 * @return
		 */
		private String generateInsertStatement(List<String> values, String valuePlaceHolder)
		{
			TransactionalStringBuilder bldr = new TransactionalStringBuilder(SPACE);
			bldr.append("INSERT INTO");
			bldr.append(tableName);
			bldr.append("(");
			// Columns:
			bldr.openTransaction(", ");
			for(SQLColumn<?, ?> sqlCol : sqlColumns.values())
				bldr.append(sqlCol.name);
			bldr.commitTransaction(false);
			// Values:
			bldr.append(") VALUES (", false);
			bldr.openTransaction(", ");
			for(int v = 0, s = sqlColumns.size(); v < s; v++)
				bldr.append(values != null ? values.get(v) : valuePlaceHolder);
			bldr.commitTransaction(false);
			bldr.append(");", false);
			return bldr.toString();
		}
		
		/**
		 * May be overriden
		 * 
		 * @param record
		 * @throws DBException
		 */
		public void update(Record record) throws DBException
		{
			if(!isInDB())
				throw new DBException("Update failed: " + toString() + " does not exist in database!");
			
		}
		
		protected String generateUpdateStatement(List<String> values)
		{
			if(values.size() != sqlColumns.size())
				throw new IllegalArgumentException("Invalid number of values");
			return generateUpdateStatement(values, null);
		}
		
		protected String generateUpdateStatement(String valuePlaceHolder)
		{
			return generateUpdateStatement(null, valuePlaceHolder);
		}
		
		/**
		 * @param values - if null valuePlaceHolder should not be
		 * @param valuePlaceHolder - if null values should not be
		 * @return
		 */
		private String generateUpdateStatement(List<String> values, String valuePlaceHolder)
		{
			TransactionalStringBuilder bldr = new TransactionalStringBuilder(SPACE);
			bldr.append("UPDATE");
			bldr.append(tableName);
			bldr.append("SET");
			
			
//			PrimaryKey pk = schema.getPrimaryKey();
//			AndConstraint whereConstraint = new 
//			for(SQLColumn<?, ?> sqlCol : sqlColumns.values())
//			{
//				if()
//			}
			
			// Columns & values (except primary key parts):
//			bldr.openTransaction(", ");
//			for(SQLColumn<?, ?> sqlCol : sqlColumns.values())
//				bldr.append(sqlCol.name);
//			bldr.commitTransaction(false);
//			// Values:
//			bldr.append(") VALUES (", false);
//			bldr.openTransaction(", ");
//			for(int v = 0, s = sqlColumns.size(); v < s; v++)
//				bldr.append(values != null ? values.get(v) : valuePlaceHolder);
//			bldr.commitTransaction(false);
//			bldr.append(");", false);
			return bldr.toString();
		}
		
		/**
		 * May be overriden
		 * 
		 * @param record
		 * @throws DBException
		 */
		public void delete(Record record) throws DBException
		{
			if(!isInDB())
				return; // the table doesn't exist in the database so there is no record to delete
			// TODO
		}
		
		public String toString()
		{
			return "database table '" + tableName + "'";
		}

	}
	
	/**
	 * @author mstevens
	 *
	 * @param <SQLType>
	 * @param <SapType>
	 */
	public abstract class SQLColumn<SQLType, SapType>
	{
		
		static public final char QUALIFIED_COLUMN_NAME_SEPARATOR = '_'; 
		
		public final String name;
		protected final String type;
		protected final String constraint;
		protected final ColumnPointer sourceColumnPointer;
		protected final TypeMapping<SQLType, SapType> mapping;
		
		/**
		 * @param type
		 * @param constraint
		 * @param sourceSchema
		 * @param sourceColumn
		 * @param mapping
		 */
		public SQLColumn(String type, String constraint, Schema sourceSchema, Column<SapType> sourceColumn, TypeMapping<SQLType, SapType> mapping)
		{
			this(type, constraint, new ColumnPointer(sourceSchema, sourceColumn), mapping);
		}
		
		/**
		 * @param type
		 * @param constraint
		 * @param sourceColumnPointer
		 * @param mapping
		 */
		private SQLColumn(String type, String constraint, ColumnPointer sourceColumnPointer, TypeMapping<SQLType, SapType> mapping)
		{
			this(sourceColumnPointer.getQualifiedColumnName(QUALIFIED_COLUMN_NAME_SEPARATOR), type, constraint, sourceColumnPointer, mapping);
		}
		
		/**
		 * @param name
		 * @param type
		 * @param constraint
		 * @param sourceColumnPointer
		 * @param sourceMapping
		 */
		@SuppressWarnings("unchecked")
		public SQLColumn(String name, String type, String constraint, ColumnPointer sourceColumnPointer, TypeMapping<SQLType, SapType> mapping)
		{
			this.name = sanitiseIdentifier(name);
			this.type = type;
			this.constraint = constraint;
			this.sourceColumnPointer = sourceColumnPointer;
			this.mapping = mapping != null ? mapping : (TypeMapping<SQLType, SapType>) TypeMapping.<SQLType> Transparent();
		}

		/**
		 * @param value
		 * @param quotedIfNeeded
		 * @return
		 */
		public String sapToLiteral(SapType value, boolean quotedIfNeeded)
		{
			return sqlToLiteral(value != null ? mapping.toSQLType(value) : null, quotedIfNeeded);
		}
		
		/**
		 * @param value
		 * @param quotedIfNeeded
		 * @return
		 */
		@SuppressWarnings("unchecked")
		public String sapelliObjectToLiteral(Object value, boolean quotedIfNeeded)
		{
			return sqlToLiteral(value != null ? mapping.toSQLType((SapType) value) : null, quotedIfNeeded);
		}
		
		/**
		 * @param record
		 * @param quotedIfNeeded
		 * @return
		 */
		public String retrieveAsLiteral(Record record, boolean quotedIfNeeded)
		{
			return sqlToLiteral(retrieve(record), quotedIfNeeded);
		}
		
		/**
		 * @param record
		 * @return
		 */
		@SuppressWarnings("unchecked")
		public SQLType retrieve(Record record)
		{
			SapType value = (SapType) sourceColumnPointer.retrieveValue(record);
			return value != null ? mapping.toSQLType(value) : null;
		}
		
		/**
		 * @param record
		 * @param value
		 */
		public void store(Record record, SQLType value)
		{
			sourceColumnPointer.getColumn().storeObject(sourceColumnPointer.getRecord(record, true), value != null ? mapping.toSapelliType(value) : null);
		}
		
		/**
		 * @param value
		 * @param quotedIfNeeded
		 * @return
		 */
		protected String sqlToLiteral(SQLType value, boolean quotedIfNeeded)
		{
			if(value != null)
				return (quotedIfNeeded && needsQuotedLiterals() ?
							getQuoteChar() + value.toString().replace(getQuoteChar(), getQuoteEscape()) + getQuoteChar() :
							value.toString());
			else
				return getNullString();
		}
		
		/**
		 * To be overridden when quotes are needed (e.g. on Strings)
		 * 
		 * @return
		 */
		protected boolean needsQuotedLiterals()
		{
			return false;
		}
		
		protected abstract String getNullString();

		protected abstract String getQuoteChar();
		
		protected abstract String getQuoteEscape();
		
	}
	
	/**
	 * @author mstevens
	 *
	 */
	static public abstract class TypeMapping<SQLType, SapType>
	{
		
		static public <SameType> TypeMapping<SameType, SameType> Transparent()
		{
			return new TypeMapping<SameType, SameType>()
			{

				@Override
				public SameType toSQLType(SameType value)
				{
					return value;
				}

				@Override
				public SameType toSapelliType(SameType value)
				{
					return value;
				}
			};
		}

		/**
		 * @param value assumed to be non-null!
		 * @return
		 */
		public abstract SQLType toSQLType(SapType value);
		
		/**
		 * @param value assumed to be non-null!
		 * @return
		 */
		public abstract SapType toSapelliType(SQLType value);
		
	}
	
	/**
	 * @author mstevens
	 *
	 */
	public abstract class TableFactory
	{
		
		public abstract STable generateTable(Schema schema);
		
	}
	
	/**
	 * This TableFactory implementation uses a column visitor and assumes all
	 * non-composite Sapelli columns will be represented by by exactly 1 SQLColumn.
	 * Composites (i.e. RecordColumns) will be mapped to sets of SQLColumns, each
	 * corresponding to exactly 1 subcolumn.
	 * 
	 * The different kinds of ListColumns are all handled the same.
	 * 
	 * @author mstevens
	 */
	public abstract class BasicTableFactory extends TableFactory implements ColumnVisitor
	{
		
		protected Schema schema;
		protected List<Index> indexesToProcess;
		protected STable table;
		
		@Override
		public STable generateTable(Schema schema)
		{
			this.schema = schema;
			indexesToProcess = new ArrayList<Index>(schema.getIndexes(true)); // copy list of all indexes, including the primary key
			initialiseTable(); // instantiates the table object
			
			schema.accept(this); // traverse schema --> columnSpecs will be added to TableSpec
			table.setTableConstraint(getTableConstraints()); // generate additional table constraints
			
			/* indexes that will be implicitly created as part of the table (and columns) definitions
			 * will have been removed from the list at this point. Any indexes that are left will have
			 * to be created explicitly: */
			table.setExplicitIndexes(indexesToProcess);
			return table;
		}
		
		/**
		 * Initialises the table variable with a new STable object.
		 * Assumes schema has been set beforehand!
		 * 
		 * @return
		 */
		protected abstract void initialiseTable();
				
		/**
		 * This method is assumed to be called only after all columns have been added to the table!
		 * 
		 * @return
		 */
		protected abstract List<String> getTableConstraints();
		
		/**
		 * TODO implement ListColumns using normalisation:
		 * 		generate Schema for a "subtable" with FK to this one --> generate table for it ... etc.
		 * 		Table will then how a ListCol -> Table map ...
		 * 		This will require additional creates/inserts/updates/deletes to be executed...
		 * 		Difficult but not impossible!
		 * 
		 * @param listCol
		 */
		public abstract <L extends List<T>, T> void visitListColumn(ListColumn<L, T> listCol);
		
		@Override
		public void visit(IntegerListColumn intListCol)
		{
			visitListColumn(intListCol);
		}
		
		@Override
		public void visit(PolygonColumn polyCol)
		{
			visitListColumn(polyCol);
		}
		
		@Override
		public void visit(LineColumn lineCol)
		{
			visitListColumn(lineCol);
		}
		
		@Override
		public boolean allowOrientationSelfTraversal()
		{
			return true;
		}
		
		@Override
		public boolean allowLocationSelfTraversal()
		{
			return true;
		}
		
		@Override
		public boolean allowForeignKeySelfTraversal()
		{
			return true;
		}
		
		@Override
		public boolean skipNonBinarySerialisedOrientationSubColumns()
		{
			return false;
		}
		
		@Override
		public boolean skipNonBinarySerialisedLocationSubColumns()
		{
			return false;
		}
		
		@Override
		public boolean includeVirtualColumns()
		{
			return false;
		}
		
		@Override
		public <VT, ST> void visit(VirtualColumn<VT, ST> virtCol)
		{
			// never called
		}
		
		@Override
		public void visit(OrientationColumn orCol)
		{
			// never called
		}
		
		@Override
		public void visit(LocationColumn locCol)
		{
			// never called
		}
		
		@Override
		public void visit(ForeignKeyColumn foreignKeyCol)
		{
			// never called
		}
		
		@Override
		public void enter(RecordColumn<?> recordCol)
		{
			// do nothing
		}
		
		@Override
		public void leave(RecordColumn<?> recordCol)
		{
			// do nothing	
		}
		
	}
	
	/**
	 * @author mstevens
	 *
	 */
	protected class SelectionClauseBuilder implements ConstraintVisitor
	{
		
		STable table;
		TransactionalStringBuilder bldr;
		String valuePlaceHolder;
		List<String> arguments;
		boolean quoteArgumentsIfNeeded;
		private DBException exception = null;
		
		/**
		 * Creates a SelectionClauseBuilder that will produce a selection clause with literal values.
		 * 
		 * @param table
		 * @param constraint
		 * @param valuePlaceHolder placeholder for parameters, null if literal clause is being generated
		 */
		public SelectionClauseBuilder(STable table, Constraint constraint)
		{
			this(table, constraint, null, true);
		}
		
		/**
		 * Creates a SelectionClauseBuilder that will produce a selection clause with parameterised values (unless valuePlaceHolder is null)
		 * 
		 * @param table
		 * @param constraint
		 * @param valuePlaceHolder
		 * @param quoteArgumentsIfNeeded
		 */
		public SelectionClauseBuilder(STable table, Constraint constraint, String valuePlaceHolder, boolean quoteArgumentsIfNeeded)
		{
			this.table = table;
			this.valuePlaceHolder = valuePlaceHolder;
			this.quoteArgumentsIfNeeded = quoteArgumentsIfNeeded;
			if(isParameterised())
				this.arguments = new ArrayList<String>();
			this.bldr = new TransactionalStringBuilder(SPACE); // pass space as connective
			if(constraint != null)
				constraint.accept(this);
		}
		
		protected boolean isParameterised()
		{
			return valuePlaceHolder != null;
		}
		
		@Override
		public void visit(AndConstraint andConstr)
		{
			visitAndOr(true, andConstr.getSubConstraints());
		}

		@Override
		public void visit(OrConstraint orConstr)
		{
			visitAndOr(false, orConstr.getSubConstraints());	
		}
		
		private void visitAndOr(boolean and, List<Constraint> subConstraints)
		{
			// Set-up "outer" and "mid" buffers:
			bldr.openTransaction(""); // open "outer" buffer: for "(" and ")" (empty string as connective)
			bldr.append("(");
			bldr.openTransaction(and ? "AND" : "OR"); // open "mid" buffer: for subConstraints & AND/OR connectives
			
			// Loop over subconstraints:
			Iterator<Constraint> iterConstr = subConstraints.iterator();
			while(iterConstr.hasNext())
			{
				bldr.openTransaction(SPACE); // open "inner" buffer: for individual subConstraint
				iterConstr.next().accept(this); // visit subConstraint
				bldr.commitTransaction(); // commit "inner" buffer, result is added to "mid" buffer and connective is insert when needed
			}
			
			// Handle buffers:
			if(!bldr.isCurrentTransactionEmpty()) // check if "mid" buffer has content
			{
				bldr.commitTransaction(); // commit "mid" buffer
				bldr.append(")");
				bldr.commitTransaction(); // commit "outer" buffer
			}
			else
				bldr.rollbackTransactions(2); // discard "mid" and "outer" buffer (this And/Or constraint did not produce any SQL)
		}
		
		@Override
		public void visit(NotConstraint notConstr)
		{
			// Set-up buffers:
			bldr.openTransaction(""); // open "outer" buffer: for "NOT (" and ")" (empty string as connective)
			bldr.append("NOT (");
			bldr.openTransaction(SPACE); // open "inner" buffer for negated constraint
			
			// Visit negated constraint:
			notConstr.getNegatedConstraint().accept(this);
			
			// Handle buffers:
			if(!bldr.isCurrentTransactionEmpty()) // check if "inner" buffer has content
			{
				bldr.commitTransaction(); // commit "inner" buffer
				bldr.append(")");
				bldr.commitTransaction(); // commit "outer" buffer
			}
			else
				bldr.rollbackTransactions(2); // discard "mid" and "outer" buffer (this Not constraint did not produce any SQL)
		}

		@Override
		public void visit(EqualityConstraint equalityConstr)
		{
			ColumnPointer cp = equalityConstr.getColumnPointer();
			SColumn sqlCol = table.getSQLColumn(cp);
			if(sqlCol != null)
			{	// Equality constraint on leaf column...
				String literalValue = sqlCol.sapelliObjectToLiteral(equalityConstr.getValue(), quoteArgumentsIfNeeded);
				bldr.append(sqlCol.name);
				bldr.append(getComparisonOperator(RuleConstraint.Comparison.EQUAL));
				if(isParameterised())
				{
					bldr.append(valuePlaceHolder);
					arguments.add(literalValue);
				}
				else
					bldr.append(literalValue);
			}
			else if(cp.getColumn() instanceof RecordColumn<?> && /* just to be sure: */ equalityConstr.getValue() instanceof Record)
			{	// Equality constraint on composite column...
				List<SColumn> subSqlCols = table.getSQLColumns((RecordColumn<?>) cp.getColumn());
				if(subSqlCols != null)
				{
					Record valueRecord = (Record) equalityConstr.getValue();
					AndConstraint andConstr = new AndConstraint();
					for(SColumn subSqlCol : subSqlCols)
						andConstr.addConstraint(new EqualityConstraint(subSqlCol.sourceColumnPointer, subSqlCol.sourceColumnPointer.retrieveValue(valueRecord)));
					visit(andConstr);
				}
			}
			else
			{
				exception = new DBException("Failed to generate SQL for equalityConstraint on column " + equalityConstr.getColumnPointer().getQualifiedColumnName(table.schema));
			}
		}

		@Override
		public void visit(RuleConstraint ruleConstr)
		{
			SColumn sqlCol = table.getSQLColumn(ruleConstr.getColumnPointer());
			if(sqlCol == null)
			{
				new DBException("Failed to generate SQL for ruleConstraint on column " + ruleConstr.getColumnPointer().getQualifiedColumnName(table.schema));
				return;
			}
			String literalValue = sqlCol.sapelliObjectToLiteral(ruleConstr.getValue(), quoteArgumentsIfNeeded);
			bldr.append(sqlCol.name);
			bldr.append(getComparisonOperator(ruleConstr.getComparison()));
			if(isParameterised())
			{
				bldr.append(valuePlaceHolder);
				arguments.add(literalValue);
			}
			else
				bldr.append(literalValue);
		}
		
		public String getWhereClause() throws DBException
		{
			String clause = getClause();
			return clause.isEmpty() ? "" : "WHERE " + clause;
		}
		
		public String getClauseOrNull() throws DBException
		{
			String clause = getClause();
			return clause.isEmpty() ? null : clause;
		}
		
		public String getClause() throws DBException
		{
			if(exception != null)
				throw exception;
			return bldr.toString();
		}
		
		public String[] getArguments()
		{
			if(isParameterised())
				return arguments.toArray(new String[arguments.size()]);
			else
				return null;
		}
		
	}

}
