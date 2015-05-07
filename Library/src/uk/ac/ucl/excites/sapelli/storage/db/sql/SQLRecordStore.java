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
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBConstraintException;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBPrimaryKeyException;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.shared.util.TransactionalStringBuilder;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.ListColumn.Simple;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.RecordColumn;
import uk.ac.ucl.excites.sapelli.storage.model.RecordReference;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.LineColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.LocationColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.OrientationColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.PolygonColumn;
import uk.ac.ucl.excites.sapelli.storage.model.indexes.Index;
import uk.ac.ucl.excites.sapelli.storage.queries.ExtremeValueRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.FirstRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Order;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery.Executor;
import uk.ac.ucl.excites.sapelli.storage.queries.Source;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.AndConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.ConstraintVisitor;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.EqualityConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.NotConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.OrConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.RuleConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.RuleConstraint.Comparison;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * @author mstevens
 *
 * @param <SRS>
 * @param <STable>
 * @param <SColumn>
 * 
 */
public abstract class SQLRecordStore<SRS extends SQLRecordStore<SRS, STable, SColumn>, STable extends SQLRecordStore<SRS, STable, SColumn>.SQLTable, SColumn extends SQLRecordStore<SRS, STable, SColumn>.SQLColumn<?, ?>> extends RecordStore
{
	
	static protected final String SPACE = " ";
	
	private final int version;
	private STable modelsTable;
	private STable schemataTable;
	
	/**
	 * Maps references to(!) "schemaMetaRecords" (records of Schema.META_SCHEMA, each describing a Schema) to the table corresponding to the described Schema
	 */
	private final Map<RecordReference, STable> tables;
	
	
	/**
	 * If non-null (all) SQL statements/queries will use parameters instead of literal values
	 */
	private final String valuePlaceHolder;

	/**
	 * @param client
	 * @param version current (targetted) version of the database, if existing database is older it will be upgrader (see {@link #initialise(boolean, int, Upgrader)}).
	 * @param valuePlaceHolder - may be null if no parameters are to be used on (all) SQL statements/queries (only literal values)
	 */
	public SQLRecordStore(StorageClient client, int version, String valuePlaceHolder)
	{
		super(client, true); // make use of roll-back tasks
		this.version = version;
		this.tables = new HashMap<RecordReference, STable>();
		this.valuePlaceHolder = valuePlaceHolder;
	}
	
	/**
	 * Must be called from subclass constructor!
	 * 
	 * @param newDB whether or not the database file is new (i.e. empty)
	 * @throws DBException
	 */
	protected void initialise(boolean newDB, int dbVersion, Upgrader upgrader) throws DBException
	{
		// create the Models and Schemata tables if they doesn't exist yet (i.e. for a new database)
		if(newDB)
			startTransaction();
		try
		{
			this.modelsTable = getTable(Model.MODEL_SCHEMA, newDB);
			this.schemataTable = getTable(Model.META_SCHEMA, newDB);
			
			// Upgrade if necessary:
			if(dbVersion < version)
				upgrader.upgrade(this, dbVersion, version);
		}
		catch(DBException e)
		{
			rollbackTransactions();
			throw e;
		}
		if(newDB)
			commitTransaction();
	}
	
	protected abstract void executeSQL(String sql) throws DBException;
	
	protected abstract int executeSQLReturnAffectedRows(String sql) throws DBException;
	
	protected abstract String sanitiseIdentifier(String identifier);
	
	protected String getTableName(Schema schema)
	{
		return sanitiseIdentifier(client.getTableName(schema));
	}
	
	/**
	 * Checks whether a table for the given schema exists in the database.
	 * 
	 * @param schema
	 * @return
	 */
	protected boolean doesTableExist(Schema schema)
	{
		return doesTableExist(getTableName(schema));
	}
	
	/**
	 * Checks whether a table with the given name exists in the database.
	 * 
	 * @param tableName
	 * @return
	 */
	protected abstract boolean doesTableExist(String tableName);

	/**
	 * @param schema
	 * @param createWhenNotInDB
	 * @return
	 * @throws DBException
	 */
	protected STable getTable(Schema schema, boolean createWhenNotInDB) throws DBException
	{
		// Check known tables:
		STable table = null;
		RecordReference schemaMetaRecordRef = null;
		if(schema == Model.MODEL_SCHEMA)
			table = modelsTable; // may still be null if getTable() was called from initialise()
		else if(schema == Model.META_SCHEMA)
			table = schemataTable; // may still be null if getTable() was called from initialise()
		else
		{
			schemaMetaRecordRef = Schema.GetMetaRecordReference(schema); // get reference to schemaMetaRecord
			table = tables.get(schemaMetaRecordRef); // lookup in tables cache
		}
		
		// If not found, generate new SQLTable object for the Schema:
		if(table == null)
		{
			table = getTableFactory().generateTable(schema);
			if(schema != Model.MODEL_SCHEMA && schema != Model.META_SCHEMA) // the "tables" map is only for tables of "real" (non-meta) schemata!
				tables.put(schemaMetaRecordRef, table);
		}
		
		// If requested then create the actual table in the database if it is not there:
		if(createWhenNotInDB && !table.isInDB())
		{
			startTransaction();
			try
			{
				// Create the table itself in the DB:
				table.create();

				// Register the schema & its model; unless the table it is the modelsTable or the schemataTable itself:
				if(schema != Model.MODEL_SCHEMA && schema != Model.META_SCHEMA)
				{
					if(!modelsTable.isRecordInDB(Model.GetModelRecordReference(schema.getModel()))) // check if model is already known (due to one of its other schemata being present)
						modelsTable.insert(Model.GetModelRecord(schema.getModel()));
					schemataTable.insert(Schema.GetMetaRecord(schema));
				}
			}
			catch(Exception e)
			{
				rollbackTransactions();
				throw new DBException("Exception upon creating and registering " + table.toString(), e);
			}
			commitTransaction();
		}
		
		return table;
	}

	/**
	 * Finds schema tables that have become empty and drops them, along with
	 * deleting the corresponding row from the Schemata table.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#cleanup()
	 */
	protected void cleanup() throws DBException
	{
		// Find empty tables & release all table resources:
		List<STable> emptyTables = null;
		for(STable table : tables.values())
		{
			try
			{
				// Check if table is empty:
				if(table.isInDB() && table.isEmpty())
				{
					if(emptyTables == null)
						emptyTables = new ArrayList<STable>();
					emptyTables.add(table);
				}
				// Release table resources:
				table.release();
			}
			catch(DBException dbE)
			{
				dbE.printStackTrace(System.err);
				continue;
			}
		}
		// Release resources on "system tables":
		schemataTable.release();
		modelsTable.release();
		
		// Clean up empty tables:
		if(emptyTables != null)
		{
			startTransaction();
			try
			{
				Set<Model> possiblyRemovableModels = new HashSet<Model>();
				// Forget schemata & drop tables:
				for(STable emptyTable : emptyTables)
				{
					RecordReference schemaMetaRecordRef = Schema.GetMetaRecordReference(emptyTable.schema);
					
					// Unregister (i.e. "forget") the schema (which the table corresponds to) in the schemataTable:
					schemataTable.delete(schemaMetaRecordRef);
					
					// Drop the table itself:
					emptyTable.drop();
					
					// Remember model:
					possiblyRemovableModels.add(emptyTable.schema.getModel());
					
					// Remove from tables map:
					tables.remove(schemaMetaRecordRef);
				}
				// Forget models if none of their schemata correspond to an existing (and at this point non-empty) table in the database:
				modelLoop : for(Model model : possiblyRemovableModels)
				{
					for(Schema schema : model.getSchemata())
						if(doesTableExist(client.getTableName(schema)))
							continue modelLoop; // one of the model's schemata corresponds to a table, so we should not forget about this model
					// None of the model's schemata correspond to a table, so unregister (i.e. "forget") the model in the modelsTable:
					modelsTable.delete(Model.GetModelRecordReference(model));
				}
			}
			catch(DBException dbE)
			{
				try
				{
					rollbackTransactions();
				}
				catch(DBException dbE2)
				{
					dbE2.printStackTrace(System.err);
				}
				throw dbE;
			}
			commitTransaction();
		}
	}
	
	protected List<Schema> getAllKnownSchemata()
	{
		return getAllKnownSchemataExcept(Collections.<Schema> emptySet());
	}
	
	protected List<Schema> getAllKnownSchemataExcept(Set<Schema> skipSchemata)
	{
		try
		{
			// Check if we know any schema at all:
			if(schemataTable == null || schemataTable.isEmpty())
				return Collections.<Schema> emptyList(); // we're done here
			
			// Construct constraint to filter out undesired schemata:
			AndConstraint filterSkipSchemata = new AndConstraint();
			for(Schema cachedSchema : skipSchemata)
				filterSkipSchemata.addConstraint(Schema.GetMetaRecordReference(cachedSchema).getRecordQueryConstraint().negate());
			
			// Query schemata table, filtering out the undesired ones:
			List<Record> schemaMetaRecords = schemataTable.select(new RecordsQuery(Source.From(Model.META_SCHEMA), filterSkipSchemata));
			if(schemaMetaRecords.isEmpty())
				return Collections.<Schema> emptyList(); // we're done here
			
			// List of schemata to return:
			List<Schema> schemata = new ArrayList<Schema>(schemaMetaRecords.size());
			// We cache all model objects we come across to avoid having to needlessly deserialise them from model records:
			Map<Long, Model> modelCache = new HashMap<Long, Model>(); 
			
			// Loop through schemaMetaRecords and obtain a Schema object for each one:
			for(Record schemaMetaRecord : schemaMetaRecords)
			{
				Schema schema;
				// First consult the tables cache:
				STable table = tables.get(schemaMetaRecord);
				if(table != null)
				{	// Got table corresponding to schemaMetaRecord, get Schema object from it 
					schema = table.schema;
					modelCache.put(schema.model.id, schema.model); // remember model in cache
				}
				else
				{	// No cached table, we will have to consult the models ...
					RecordReference modelRef = Model.META_MODEL_ID_COLUMN.retrieveValue(schemaMetaRecord);
					// ... first check the model cache:
					Model model = modelCache.get(Model.MODEL_ID_COLUMN.retrieveValue(modelRef));
					if(model == null)
					{	// model is not in cache, so query the models table:
						model = Model.FromModelRecord(modelsTable.select(modelRef.getRecordQuery())); // model object obtained by deserialising model record
						modelCache.put(model.id, model); // remember model in cache
					}
					// Get the schema from the model object:
					schema = model.getSchema(Model.META_SCHEMA_NUMBER_COLUMN.retrieveValue(schemaMetaRecord).intValue());
				}
				
				// Add schema to list:
				schemata.add(schema);
			}
			
			// Return the schemata:
			return schemata;
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
			return Collections.<Schema> emptyList();
		}
	}
	
	/**
	 * Returns a Schema object that corresponds to the stored version of the given schema.
	 * Only to be used for upgrade purposes.
	 * 
	 * @param schema
	 * @return the stored version of the schema, or null if no records of this schema are currently stored
	 */
	protected Schema getStoredVersion(Schema schema)
	{
		try
		{
			Record schemaMetaRecord = Schema.GetMetaRecord(schema);
			RecordReference modelRef = Model.META_MODEL_ID_COLUMN.retrieveValue(schemaMetaRecord);
			Model model = Model.FromModelRecord(modelsTable.select(modelRef.getRecordQuery())); // model object obtained by deserialising model record
			return model.getSchema(Model.META_SCHEMA_NUMBER_COLUMN.retrieveValue(schemaMetaRecord).intValue());
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
			return null;
		}
	}
	
	protected abstract TableFactory getTableFactory();
	
	protected Collection<Schema> getSchemata(Source source)
	{
		return	(source.isAny() ?
					getAllKnownSchemata() :
					(source.isByInclusion() ?
						source.getSchemata() :
						getAllKnownSchemataExcept(source.getSchemata())));
	}
	
	/**
	 * Default implementation for SQL databases: first do a SELECT based on the key to verify whether the
	 * record exists, then do either UPDATE or INSERT.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#doStore(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	protected Boolean doStore(Record record) throws DBException, IllegalStateException
	{
		return getTable(record.getSchema(), true).store(record, true); // getTable() will create table in db if it is not there
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#doInsert(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	protected boolean doInsert(Record record) throws DBPrimaryKeyException, DBConstraintException, DBException, IllegalStateException
	{
		Boolean inserted = getTable(record.getSchema(), true).store(record, false); // getTable() will create table in db if it is not there
		if(inserted == null)
			return false; // record was already stored with identical values
		if(inserted) // new record was inserted
			return true;
		else // record existed and would have been UPDATEd if it were allowed 
			throw new DBPrimaryKeyException("This record already exists in the record store (with different values).");
	}

	@Override
	protected boolean doDelete(Record record) throws DBException
	{
		STable table = getTable(record.getSchema(), false); // no need to create the table in the db if it isn't there!
		return table.isInDB() && table.delete(record);
	}
	
	/**
	 * Deletes the record pointed to by the given reference.
	 * Overridden for increased performance.
	 * 
	 * @param recordRef
	 * @throws DBException
	 */
	@Override
	public void delete(RecordReference recordRef) throws DBException
	{
		STable table = getTable(recordRef.getReferencedSchema(), false); // no need to create the table in the db if it isn't there!
		if(table.isInDB() && table.delete(recordRef))
			client.recordDeleted(recordRef); // inform client
	}
	
	/**
	 * Deletes all records that match the query.
	 * Overridden for increased performance.
	 * 
	 * @param recordsQuery
	 * @throws DBException
	 */
	@Override
	public void delete(RecordsQuery query) throws DBException
	{
		int totalDeleted = 0;
		for(Schema s : getSchemata(query.getSource()))
		{
			try
			{
				STable table = getTable(s, false);
				if(!table.isInDB())
					continue; // table does no exist in DB, so there are no records to retrieve
				totalDeleted += table.delete(query);
			}
			catch(DBException dbE)
			{
				dbE.printStackTrace(System.err);
			}
		}
		if(totalDeleted > 0)
			client.recordsDeleted(query, totalDeleted); // inform client
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#retrieveRecords(uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery)
	 */
	@Override
	public List<Record> retrieveRecords(RecordsQuery query)
	{
		List<Record> resultAcc = null;
		// Run subqueries for each schema in the query, or all known schemata (if the query is for "any" schema):
		for(Schema s : getSchemata(query.getSource()))
		{
			try
			{
				STable table = getTable(s, false);
				if(!table.isInDB())
					continue; // table does no exist in DB, so there are no records to retrieve
				List<Record> subResult = table.select(query);
				if(!subResult.isEmpty())
				{
					if(resultAcc == null)
						resultAcc = subResult;
					else
						resultAcc.addAll(subResult);
				}
			}
			catch(DBException dbE)
			{
				dbE.printStackTrace(System.err);
			}
		}
		return resultAcc != null ? resultAcc : Collections.<Record> emptyList();
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#retrieveRecord(uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery)
	 */
	@Override
	public Record retrieveRecord(SingleRecordQuery query)
	{
		List<Record> candidates = null;
		// Run subqueries for each schema in the query, or all known schemata (if the query is for "any" schema):
		RecordsQuery recsQuery = query.getRecordsQuery();
		for(Schema s : getSchemata(recsQuery.getSource()))
		{
			try
			{
				STable table = getTable(s, false);
				if(!table.isInDB())
					continue; // table does no exist in DB, so there are no records to retrieve
				Record candidate = table.select(query);
				if(candidate != null)
				{
					if(candidates == null)
						candidates = new ArrayList<Record>();
					candidates.add(candidate);
				}
			}
			catch(DBException dbE)
			{
				dbE.printStackTrace(System.err);
			}
		}
		return query.execute(candidates, false); // reduce to 1 record (execute() will return null when passed a null list)
	}
	
	protected abstract String getNullString();
	
	protected abstract char getQuoteChar();
	
	protected abstract String getQuoteEscapeString();
	
	/**
	 * Get subclasses may need to override this because some SQL dialects use != instead of <> (Note: SQLite supports both)
	 * 
	 * @param comparison
	 * @return
	 */
	protected String getComparisonOperator(RuleConstraint.Comparison comparison)
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
		
		private Boolean existsInDB;
		private TableCreationHelper creator;
		
		/**
		 * Mapping of Sapelli ColumnPointers (usually leaf columns) to corresponding  SQLColumns.
		 */
		public final Map<ColumnPointer, SColumn> sqlColumns;
		
		/**
		 * Mapping of composite Sapelli columns to a list of SQLColumns which they correspond to.
		 * E.g. Location --> (Lat, Lon, ...) 
		 */
		public final Map<RecordColumn<?>, List<SColumn>> composite2SqlColumns;
		
		/**
		 * The auto-incrementing primary key column as (a Sapelli Column, not an S/SQLColumn), will be null if there is none
		 */
		protected final IntegerColumn autoIncrementKeySapColumn;
		protected SColumn autoIncrementKeySQLColumn;
		
		public SQLTable(Schema schema)
		{
			this.tableName = getTableName(schema);
			this.schema = schema;
			// Init collections:
			sqlColumns = new LinkedHashMap<ColumnPointer, SColumn>(); // we use a LHP to preserve column order!
			composite2SqlColumns = new HashMap<RecordColumn<?>, List<SColumn>>();
			// Deal with auto-increment key:
			this.autoIncrementKeySapColumn = schema.getAutoIncrementingPrimaryKeyColumn();
		}
		
		/**
		 * @param sqlColumn
		 */
		public void addColumn(SColumn sqlColumn)
		{
			ColumnPointer sourceCP = sqlColumn.sourceColumnPointer;
			if(sourceCP == null || sourceCP.getColumn() == null)
				throw new IllegalArgumentException("SQLColumn needs a valid sourceColumnPointer in order to be added to a SQLTable instance");
			
			// Add SQLColumn:
			sqlColumns.put(sourceCP, sqlColumn);
			
			// Deal with AutoIncr...
			if(sourceCP.getColumn() == autoIncrementKeySapColumn)
				this.autoIncrementKeySQLColumn = sqlColumn;
			
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
		
		public boolean isInDB()
		{
			if(existsInDB == null)
				existsInDB = doesTableExist(tableName);
			return existsInDB;
		}
		
		/**
		 * Creates the table (+ any indexes) in the database.
		 * Assumes the table does not already exist in the database! (caller must therefore first call isInDB())
		 * 
		 * @param factory
		 * @throws DBException
		 */
		public void create() throws DBException
		{		
			// Get creator if needed:
			if(creator == null)
				creator = getTableCreationHelper();
			
			if(isInTransaction())
			{	// this means the table creation might be rolled-back...
				final TableCreationHelper holdCreator = creator;
				addRollbackTask(new RollbackTask()
				{
					@Override
					public void run() throws DBException
					{	// If this code run that means the table wasn't created in the DB after all, so...
						//	mark table as non-existing in DB:
						existsInDB = false;
						//	and re-set the creator so it doesn't have to be generated again:
						creator = holdCreator;
					}
				});
			}
			
			// Create the table & indexes:
			creator.createTableAndIndexes();
			// Note: if there is an exception the lines below will not be executed but the roll-back task above will...
			
			// Now the table exists...
			existsInDB = true; // !!!
			
			// Discard the creator to limit memory consumption:
			creator = null;
		}
		
		protected abstract TableCreationHelper getTableCreationHelper();
		
		public SColumn getSQLColumn(ColumnPointer sapColumnPointer)
		{
			return getSQLColumn(sapColumnPointer.getColumn());
		}
		
		public SColumn getSQLColumn(Column<?> sapColumn)
		{
			return sqlColumns.get(new ColumnPointer(schema, sapColumn));
		}
		
		public List<SColumn> getSQLColumns(Column<?> sapColumn)
		{
			if(sapColumn instanceof RecordColumn)
				return composite2SqlColumns.get((RecordColumn<?>) sapColumn);
			else
				return Collections.singletonList(getSQLColumn(sapColumn));
		}
		
		/**
		 * Store a record by INSERTing, if it is new, or UPDATEing if it existed.
		 * 
		 * @param record
		 * @param updateAllowed whether or not updates are allowed
		 * @return whether the record was new (i.e. it was INSERTed; returns {@code true}); was, or would have been if allowed, modified (i.e. UPDATEd; returns {@code false}); or neither (i.e. the exact same record was already stored; returns {@code null})
		 * @throws DBConstraintException
		 * @throws DBException
		 * @throws IllegalStateException when the columns that are part of the primary key have not all been assigned a value
		 */
		public Boolean store(Record record, boolean updateAllowed) throws DBPrimaryKeyException, DBConstraintException, DBException, IllegalStateException
		{
			if(!isRecordInDB(record))
			{
				insert(record);
				return true;
			}
			else if(!updateAllowed)
				return record.hasEqualValues(select(record.getRecordQuery())) ? null : false;
			else
				return update(record) ? false : null;
		}
		
		/**
		 * Checks if the given {@link Record} instance already exists in the database table.
		 * Also works for recordReferences to records of this table's schema!
		 * 
		 * May be overridden.
		 * 
		 * @param record
		 * @return
		 * @throws NullPointerException
		 * @throws DBException
		 * @throws IllegalStateException when the columns that are part of the primary key have not all been assigned a value
		 */
		public boolean isRecordInDB(Record record) throws DBException, IllegalStateException
		{
			return	isInDB() &&
					(autoIncrementKeySapColumn == null || autoIncrementKeySapColumn.isValueSet(record)) ? 
						select(record.getRecordQuery()) != null :
						false;
		}
		
		/**
		 * Insert new record in database table.
		 * Assumes the table exists in the database!
		 * 
		 * *Must* be overridden in order to support auto incrementing keys.
		 * 
		 * @throws DBPrimaryKeyException
		 * @throws DBConstraintException
		 * @throws DBException
		 */
		@SuppressWarnings("unchecked")
		public void insert(Record record) throws DBPrimaryKeyException, DBConstraintException, DBException
		{
			// This method cannot be used on schemata with auto-incrementing PKs:
			if(autoIncrementKeySapColumn != null)
				throw new UnsupportedOperationException("Default SQLRecordStore.SQLTable#insert(Record) implementation does not support setting auto-incrementing key values.");
			//else...
			executeSQL(new RecordInsertHelper((STable) this, record).getQuery());
		}
		
		/**
		 * Update existing record in database table.
		 * Assumes the table exists in the database!
		 * 
		 * May be overridden.
		 * 
		 * Note:
		 * 	Currently the detection of _actual_ changes to the record does *not* work.
		 * 	If the UPDATE statement's WHERE clause matches an existing row that row will (at least in SQLite) be considered
		 * 	as changed/affected (i.e. this method will return true) even if the actual values remained unchanged.
		 * 	The only obvious way to fix this is to generate UPDATE statements in which the WHERE clause checks whether
		 * 	values _need_ to be updated. E.g. "UPDATE table SET col1 = "newVal1", col2 = "newVal2" WHERE id = X AND (col1 IS NOT 'newVal1' OR col2 IS NOT 'newVal2');"
		 * 	We could implement such a solution in RecordUpdateHelper but will wait until we are certain this feature will be required.
		 * @see See: <a href="http://stackoverflow.com/questions/26372449">http://stackoverflow.com/questions/26372449</a>
		 * TODO implement solution to detect actual record value changes
		 * 
		 * @param record
		 * @return whether the record was really updated or stayed unchanged (because the record that was passed is identical to the stored one)
		 * @throws DBConstraintException
		 * @throws DBException
		 */
		@SuppressWarnings("unchecked")
		public boolean update(Record record) throws DBConstraintException, DBException
		{
			return executeSQLReturnAffectedRows(new RecordUpdateHelper((STable) this, record).getQuery()) == 1;
		}
		
		/**
		 * Delete existing record (identified by a Record or RecordReference) in database table.
		 * Assumes the table exists in the database!
		 * Also works for recordReferences to records of this table's schema!
		 * 
		 * May be overridden.
		 * 
		 * @param record a {@link Record} or {@link RecordReference} instance
		 * @return whether the record was really deleted
		 * @throws DBException
		 */
		@SuppressWarnings("unchecked")
		public boolean delete(Record record) throws DBException
		{
			return executeSQLReturnAffectedRows(new RecordDeleteHelper((STable) this, record).getQuery()) == 1;
		}
		
		/**
		 * Delete existing records (identified by a RecordsQuery) in database table.
		 * Assumes the table exists in the database!
		 * 
		 * May be overridden.
		 * 
		 * @param recordsQuery
		 * @return the number of deleted records
		 * @throws DBException
		 */
		@SuppressWarnings("unchecked")
		public int delete(RecordsQuery query) throws DBException
		{
			return executeSQLReturnAffectedRows(new RecordsDeleteHelper((STable) this, query).getQuery());
		}
		
		/**
		 * Selects records from the database table based on a RecordsQuery.
		 * Assumes the table exists in the database!
		 * 
		 * @param query
		 * @return a list, possibly empty
		 * @throws DBException
		 */
		@SuppressWarnings("unchecked")
		public List<Record> select(RecordsQuery query) throws DBException
		{
			return executeRecordSelection(new RecordSelectHelper((STable) this, query));
		}
		
		/**
		 * Selects a single record from the database table based on a SingleRecordQuery.
		 * Assumes the table exists in the database!
		 * 
		 * @param query
		 * @param result record or null
		 * @throws DBException 
		 */
		public Record select(SingleRecordQuery query) throws DBException
		{
			List<Record> results = query.<List<Record>, DBException> acceptExecutor(new Executor<List<Record>, DBException>()
			{
				
				@SuppressWarnings("unchecked")
				@Override
				public List<Record> execute(ExtremeValueRecordQuery extremeValueRecordQuery) throws DBException
				{
					return executeRecordSelection(new RecordSelectHelper((STable) SQLTable.this, extremeValueRecordQuery)); 
				}
				
				@Override
				public List<Record> execute(FirstRecordQuery firstRecordQuery) throws DBException
				{	// Execute as regular select query: the RecordsQuery held by the firstRecordQuery will always be limited to 1!
					return select(firstRecordQuery.getRecordsQuery());
				}
				
			});
			
			return results != null /* just in case */ && !results.isEmpty() ? results.get(0) : null;
		}
		
		/**
		 * @return true if the table is empty (i.e. containing 0 records) or does not exist in the DB
		 * @throws DBException
		 */
		public boolean isEmpty() throws DBException
		{
			return !isInDB() || getRecordCount() == 0;
		}
		
		/**
		 * Counts the number of records currently in the database table.
		 * Assumes the table exists in the database!
		 * 
		 * @return the number of records in the table
		 * @throws DBException 
		 */
		public abstract long getRecordCount() throws DBException;
		
		/**
		 * Drop the table from the database.
		 * Assumes the table exists in the database!
		 * 
		 * May be overridden.
		 * 
		 * @throws DBException
		 */
		public void drop() throws DBException
		{
			// Release resources:
			release();
			
			if(isInTransaction())
			{	// this means the drop operation might be rolled-back...
				addRollbackTask(new RollbackTask()
				{
					@Override
					public void run() throws DBException
					{	// If this code run that means the table wasn't dropped after all, so...
						existsInDB = true;
					}
				});
			}
			
			// Perform the DROP operation:
			executeSQL(generateDropTableStatement());
			// Note: if there is an exception the line below will not be executed but the roll-back task above will...
			
			// Now the table is gone...
			existsInDB = false; // !!!
		}
		
		protected String generateDropTableStatement()
		{
			TransactionalStringBuilder bldr = new TransactionalStringBuilder(SPACE);
			bldr.append("DROP TABLE");
			bldr.append(tableName);
			bldr.append(";", false);
			return bldr.toString();
		}
		
		/**
		 * @param selection
		 * @return list of records (possibly empty)
		 * @throws DBException
		 */
		protected abstract List<Record> executeRecordSelection(RecordSelectHelper selection) throws DBException;
		
		/**
		 * Release any resources associated with this table
		 */
		public abstract void release();
		
		/* (non-Javadoc)
		 * @see java.lang.Object#toString()
		 */
		@Override
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
		public final String type;
		public final ColumnPointer sourceColumnPointer;
		protected final TypeMapping<SQLType, SapType> mapping;
		
		/**
		 * @param type
		 * @param sourceSchema
		 * @param sourceColumn
		 * @param mapping - may be null in case SQLType = SapType
		 */
		public SQLColumn(String type, Schema sourceSchema, Column<SapType> sourceColumn, TypeMapping<SQLType, SapType> mapping)
		{
			this(null, type, sourceSchema, sourceColumn, mapping);
		}

		/**
		 * @param name
		 * @param type
		 * @param sourceSchema - may be null in specific hackish cases (e.g. {@link SQLiteRecordStore#doesTableExist(String)}) and on the condition that name is not null
		 * @param sourceColumn - may be null in specific hackish cases (e.g. {@link SQLiteRecordStore#doesTableExist(String)}) and on the condition that name is not null
		 * @param mapping - may be null in case SQLType = SapType
		 */
		@SuppressWarnings("unchecked")
		public SQLColumn(String name, String type, Schema sourceSchema, Column<SapType> sourceColumn, TypeMapping<SQLType, SapType> mapping)
		{
			this.sourceColumnPointer = (sourceSchema != null && sourceColumn != null) ? new ColumnPointer(sourceSchema, sourceColumn) : null;
			this.name = sanitiseIdentifier(name != null ? name : (sourceColumnPointer.getQualifiedColumnName(QUALIFIED_COLUMN_NAME_SEPARATOR)));
			this.type = type;
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
		 * @param sapValue
		 * @return
		 */
		@SuppressWarnings("unchecked")
		protected SQLType sapelliOjectToSQL(Object sapValue)
		{
			sapValue = sourceColumnPointer != null ? sourceColumnPointer.getColumn().convert(sapValue) : sapValue;
			return sapValue != null ? mapping.toSQLType((SapType) sapValue) : null;
		}
		
		/**
		 * @param sapValue
		 * @param quotedIfNeeded
		 * @return
		 */
		public String sapelliObjectToLiteral(Object sapValue, boolean quotedIfNeeded)
		{
			return sqlToLiteral(sapelliOjectToSQL(sapValue), quotedIfNeeded);
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
			if(value != null)
				sourceColumnPointer.getColumn().storeObject(sourceColumnPointer.getRecord(record, true), mapping.toSapelliType(value));
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
							getQuoteChar() + value.toString().replace("" + getQuoteChar(), getQuoteEscapeString()) + getQuoteChar() :
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
	 * Interface for SQLTable factory implementations, responsible for generating SQLTable instances from Schemata 
	 * 
	 * @author mstevens
	 */
	public abstract class TableFactory
	{
		
		/**
		 * Generate a SQLTable for a Schema
		 * 
		 * @param schema
		 * @return
		 * @throws DBException
		 */
		public abstract STable generateTable(Schema schema) throws DBException;
		
	}
	
	/**
	 * Default TableFactory implementation.
	 * 
	 * It uses a column visitor and assumes all non-composite Sapelli columns will be
	 * represented by by exactly 1 SQLColumn.
	 * Composites (i.e. RecordColumns) will be mapped to sets of SQLColumns, each
	 * corresponding to exactly 1 subcolumn.
	 * 
	 * The different kinds of ListColumns are all handled the same.
	 * 
	 * @author mstevens
	 */
	public abstract class BasicTableFactory extends TableFactory implements ColumnVisitor
	{
		
		protected STable table;
		
		@Override
		public STable generateTable(Schema schema) throws DBException
		{
			table = createTable(schema);
			
			// Traverse schema:
			schema.accept(this); // generates SQLColumns which get added to the table
			
			return table;
		}
		
		/**
		 * Instantiates and returns a new SQLTable object
		 * 
		 * @return
		 * @throws DBException 
		 */
		protected abstract STable createTable(Schema schema) throws DBException;
		
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
		public <T> void visit(Simple<T> simpleListCol)
		{
			visitListColumn(simpleListCol);
		}
		
		@Override
		public void visit(IntegerListColumn intListCol)
		{
			visitListColumn(intListCol);
		}
		
		@Override
		public void visit(BooleanListColumn boolListCol)
		{
			visitListColumn(boolListCol);
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
			// does nothing
		}
		
		@Override
		public void leave(RecordColumn<?> recordCol)
		{
			// does nothing
		}
		
	}
	
	/**
	 * Helper class to create a table (along with any indexes) in the database.
	 * 
	 * Upon construction it generate and (temporarily) holds on to all information
	 * (mostly partial SQL expressions) necessary to create the table and indexes.
	 * 
	 * After the table has been created the helper object is no longer needed to use the table.
	 * 
	 * @author mstevens
	 */
	public abstract class TableCreationHelper
	{
	
		protected final STable table;
		private final Map<SColumn, String> colConstraints;
		protected final List<String> tableConstraints;
		private final List<Index> explicitIndexes;
		
		public TableCreationHelper(STable table)
		{
			this.table = table;

			// Copy all indexes (incl. PK) to a modifiable list:
			List<Index> indexesToProcess = new ArrayList<Index>(table.schema.getIndexes(true)); 
			
			// Generate column constraints:
			this.colConstraints = new HashMap<SColumn, String>();
			for(SColumn sqlCol : table.sqlColumns.values())
				colConstraints.put(sqlCol, getColumnConstraint(sqlCol.sourceColumnPointer, indexesToProcess)); // processed indexes are removed from list
			
			// Generate any additional table constraints:
			tableConstraints = new ArrayList<String>();
			addTableConstraints(indexesToProcess); // processed indexes are removed from list
				
			/* Indexes that will be implicitly created as part of the table (and columns) definitions
			 * will have been removed from the list at this point. Any indexes that are left will have
			 * to be created explicitly: */
			this.explicitIndexes = indexesToProcess; // (may be empty)
		}

		/**
		 * Generates SQL constraint expression for a column, pointed at by the given ColumnPointer
		 * 
		 * @param sourceCP 
		 * @param indexesToProcess indexes which may relate to the column, any which do and which are described entirely by the returned constraint will be removed from the list
		 * @return an SQL constraint expression for the given column
		 */
		protected abstract String getColumnConstraint(ColumnPointer sourceCP, List<Index> indexesToProcess);
		
		/**
		 * Generates SQL constraint expressions for the table as a whole and adds them to the tableConstraints list.
		 * This method is assumed to be called only after all column constraints have been generated!
		 * 
		 * @param indexesToProcess indexes which may be expressed as a table constraint, any which could be described entirely as a table constraint will be removed from the list
		 */
		protected abstract void addTableConstraints(List<Index> indexesToProcess);
		
		/**
		 * Create table in database, as well as any explicit indexes on its columns.
		 * 
		 * Default implementation, may be overridden by subclasses
		 * 
		 * @throws DBException
		 */
		public void createTableAndIndexes() throws DBException
		{
			// Create the table:
			executeSQL(generateCreateTableStatement());
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
			bldr.append(table.tableName);
			bldr.append("(");
			bldr.openTransaction(", ");
			// Columns:
			for(SColumn sqlCol : table.sqlColumns.values())
			{
				bldr.openTransaction(SPACE);
				bldr.append(sqlCol.name);
				bldr.append(sqlCol.type);
				bldr.append(colConstraints.get(sqlCol));
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
			bldr.append(sanitiseIdentifier(table.tableName + "_" + idx.getName()));
			bldr.append("ON");
			bldr.append(table.tableName);
			bldr.append("(");
			bldr.openTransaction(", ");
			// List indexed columns:
			for(Column<?> idxCol : idx.getColumns(false))
				// idxCol may be a composite (like a ForeignKeyColumn), so loop over each SColumn that represents part of it:
				for(SColumn idxSCol : table.getSQLColumns(idxCol))
					bldr.append(idxSCol.name);
			bldr.commitTransaction(false);
			bldr.append(");", false);
			return bldr.toString();
		}
		
	}
	
	/**
	 * @author mstevens
	 *
	 */
	protected abstract class StatementHelper
	{
		
		protected final STable table;
		private final List<SColumn> parameterColumns;
		protected TransactionalStringBuilder bldr;
		protected String query;
		
		protected DBException exception = null;
		
		/**
		 * @param table
		 */
		public StatementHelper(STable table)
		{
			this.table = table;
			this.parameterColumns = isParameterised() ? new ArrayList<SColumn>() : null;
			this.bldr = new TransactionalStringBuilder(SPACE); // use SPACE as connective!
		}
		
		/**
		 * May be overridden in cases where literal values must be used despite there being a non-null valuePlaceHolder
		 * 
		 * @return
		 */
		protected boolean isParameterised()
		{
			return valuePlaceHolder != null;
		}

		protected void addParameterColumn(SColumn column)
		{
			parameterColumns.add(column);
		}
		
		public String getQuery() throws DBException
		{
			if(exception != null)
				throw exception;
			if(bldr != null)
			{
				query = bldr.toString();
				bldr = null;
			}
			return query;
		}
		
		/**
		 * @return list of columns or null if not in parameterised mode
		 */
		public List<SColumn> getParameterColumns()
		{
			return parameterColumns;
		}

	}
	
	/**
	 * Helper class to build INSERT statements (parameterised or literal)
	 * 
	 * @author mstevens
	 */
	protected class RecordInsertHelper extends StatementHelper
	{
		
		/**
		 * Parameterised
		 * 
		 * @param table
		 */
		public RecordInsertHelper(STable table)
		{
			this(table, null);
		}
		
		/** 
		 * @param table
		 * @param record a record instance (when the statement is not parameterised) or null (when it is parameterised)
		 */
		public RecordInsertHelper(STable table, Record record)
		{
			// Initialise
			super(table);
			
			// Build statement:
			bldr.append("INSERT INTO");
			bldr.append(table.tableName);
			bldr.append("(");
			// Columns names:
			bldr.openTransaction(", ");
			for(SColumn sqlCol : table.sqlColumns.values())
				if(sqlCol != table.autoIncrementKeySQLColumn) // skip auto-incrementing key
					bldr.append(sqlCol.name);
			bldr.commitTransaction(false);
			// Values:
			bldr.append(") VALUES (", false);
			bldr.openTransaction(", ");
			for(SColumn sqlCol : table.sqlColumns.values())
				if(sqlCol != table.autoIncrementKeySQLColumn) // skip auto-incrementing key
				{
					if(isParameterised())
					{
						bldr.append(valuePlaceHolder);
						addParameterColumn(sqlCol);
					}
					else
						bldr.append(sqlCol.retrieveAsLiteral(record, true));
				}
			bldr.commitTransaction(false);
			bldr.append(");", false);
		}
		
	}
	
	/**
	 * Abstract super class for operations that operate on a single record found by its primary key
	 * 
	 * @author mstevens
	 */
	protected abstract class RecordByPrimaryKeyHelper extends StatementHelper
	{
		
		protected final Set<SColumn> keyPartSqlCols;
		
		/**
		 * @param table
		 */
		public RecordByPrimaryKeyHelper(STable table)
		{
			super(table);
			keyPartSqlCols = new HashSet<SColumn>();
			for(Column<?> sapKeyPartCol : table.schema.getPrimaryKey().getColumns(false))
				// sapKeyPartCol may be a composite (like a ForeignKeyColumn), so loop over each SColumn that represents part of it:
				for(SColumn sqlKeyPartCol : table.getSQLColumns(sapKeyPartCol))
					CollectionUtils.addIgnoreNull(keyPartSqlCols, sqlKeyPartCol);
		}
		
		/**
		 * @param a record instance (when the statement is not parameterised) or null (when it is parameterised)
		 */
		protected void appendWhereClause(Record record)
		{
			bldr.append("WHERE");
			if(keyPartSqlCols.size() > 1)
			{
				bldr.append("(");
				bldr.openTransaction(" AND ");
			}
			for(SColumn keyPartSqlCol : keyPartSqlCols)
			{
				bldr.openTransaction(SPACE);
				bldr.append(keyPartSqlCol.name);
				bldr.append("=");
				if(isParameterised())
				{
					bldr.append(valuePlaceHolder);
					addParameterColumn(keyPartSqlCol);
				}
				else
					bldr.append(keyPartSqlCol.retrieveAsLiteral(record, true));
				bldr.commitTransaction();
			}
			if(keyPartSqlCols.size() > 1)
			{
				bldr.commitTransaction(false); // no space after "("
				bldr.append(")", false); // no space before ")"
			}
		}
		
	}
	
	/**
	 * Helper class to build UPDATE statements (parameterised or literal)
	 * 
	 * @author mstevens
	 */
	protected class RecordUpdateHelper extends RecordByPrimaryKeyHelper
	{
		
		/**
		 * Parameterised
		 * 
		 * @param table
		 */
		public RecordUpdateHelper(STable table)
		{
			this(table, null);
		}
		
		/**
		 * @param table
		 * @param record a record instance (when the statement is not parameterised) or null (when it is parameterised)
		 */
		public RecordUpdateHelper(STable table, Record record)
		{
			// Initialise
			super(table);
			
			// Build statement:			
			bldr.append("UPDATE");
			bldr.append(table.tableName);
			bldr.append("SET");
			// Columns names & values (except primary key parts):
			bldr.openTransaction(", ");
			for(SColumn sqlCol : table.sqlColumns.values())
				if(!keyPartSqlCols.contains(sqlCol))
				{
					bldr.openTransaction(SPACE);
					bldr.append(sqlCol.name);
					bldr.append("=");
					if(isParameterised())
					{
						bldr.append(valuePlaceHolder);
						addParameterColumn(sqlCol);
					}
					else
						bldr.append(sqlCol.retrieveAsLiteral(record, true));
					bldr.commitTransaction();
				}
			bldr.commitTransaction();
			// WHERE clause:
			appendWhereClause(record);
			bldr.append(";", false);
		}
		
	}
	
	/**
	 * Helper class to build DELETE statements (parameterised or literal) for single records
	 * 
	 * @author mstevens
	 */
	protected class RecordDeleteHelper extends RecordByPrimaryKeyHelper
	{
	
		/**
		 * Parameterised
		 * 
		 * @param table
		 */
		public RecordDeleteHelper(STable table)
		{
			this(table, null);
		}
		
		/**
		 * @param table
		 * @param record a {@link Record} or {@link RecordReference} instance
		 */
		public RecordDeleteHelper(STable table, Record record)
		{
			// Initialise
			super(table);
			
			// Build statement:			
			bldr.append("DELETE FROM");
			bldr.append(table.tableName);
			// WHERE clause:
			appendWhereClause(record);
			bldr.append(";", false);
		}
		
	}
	
	/**
	 * Abstract super class for operations that operate on a collection of records identified using a {@link RecordsQuery}
	 * 
	 * Important note regarding null comparisons in WHERE clauses:
	 * 	If the comparison value of the EqualityConstraint is null we must generate "col IS NULL" and never "col = null".
	 * 	The reason is that in SQL a comparison between a null value and any other value (including another null) using a
	 * 	logical operator (e.g. =, !=, <, etc) will result in a null, which is considered as false for the purposes of a
	 * 	where clause. The reasoning is that a null means "unknown", so the result of any comparison to a null is also
	 * 	"unknown". So while "col = null" would not cause errors no rows would ever match it.
	 * 
	 * @see http://stackoverflow.com/a/9581790/1084488
	 * 
	 * @author mstevens
	 */
	protected abstract class RecordsByConstraintsHelper extends StatementHelper implements ConstraintVisitor
	{
		
		private final List<Object> sapArguments;
		
		/**
		 * @param table
		 */
		protected RecordsByConstraintsHelper(STable table)
		{
			super(table);
			this.sapArguments = isParameterised() ? new ArrayList<Object>() : null;
		}
		
		protected void appendWhereClause(RecordsQuery recordsQuery)
		{
			if(recordsQuery.getConstraints() != null)
			{
				bldr.openTransaction();
				bldr.append("WHERE");
				bldr.openTransaction();
				recordsQuery.getConstraints().accept(this); // start visiting of constraint(s)
				if(!bldr.isCurrentTransactionEmpty())
					bldr.commitTransactions(2);
				else
					bldr.rollbackTransactions(2);
			}
		}
		
		protected void addParameterColumnAndValue(SColumn column, Object sapValue)
		{
			addParameterColumn(column);
			sapArguments.add(sapValue);
		}

		/**
		 * @return list of Objects (SapType values) or null if not in parameterised mode
		 */
		public List<Object> getSapArguments()
		{
			return sapArguments;
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
			bldr.append("(");
			bldr.openTransaction(" " + (and ? "AND" : "OR") + " "); // open outer transaction for subConstraints & AND/OR connectives
			
			// Loop over subconstraints:
			Iterator<Constraint> iterConstr = subConstraints.iterator();
			while(iterConstr.hasNext())
			{
				bldr.openTransaction(SPACE); // open inner transaction for individual subConstraint (using with SPACE as connective again)
				iterConstr.next().accept(this); // visit subConstraint
				bldr.commitTransaction(); // commit inner transaction, result is added to outer transaction with connective (AND/OR) inserted as needed
			}

			bldr.commitTransaction(false); // commit outer transaction, without inserting connective (i.e. no space after '(')
			bldr.append(")", false); // no connective inserted (i.e. no space before ')')
		}
		
		@Override
		public void visit(NotConstraint notConstr)
		{
			bldr.append("NOT (");
			bldr.openTransaction(SPACE); // open transaction for negated constraint
			
			// Visit negated constraint:
			notConstr.getNegatedConstraint().accept(this);
			
			bldr.commitTransaction(false); // commit transaction, without inserting connective (i.e. no space after '(')
			bldr.append(")", false); // no connective inserted (i.e. no space before ')')
		}

		@Override
		public void visit(EqualityConstraint equalityConstr)
		{
			ColumnPointer cp = equalityConstr.getColumnPointer();
			SColumn sqlCol = table.getSQLColumn(cp);
			if(sqlCol != null)
			{	// Equality constraint on non-composite (leaf) column...
				Object sapValue = equalityConstr.getValue();
				// TODO if we start supporting default values we may have to(?) replace a null value by the default value if there is one (unless the defaults are also put new Record instances)
				bldr.append(sqlCol.name);
				if(sapValue != null)
				{
					bldr.append(getComparisonOperator(equalityConstr.isEqual() ? Comparison.EQUAL : Comparison.NOT_EQUAL));
					if(isParameterised())
					{
						bldr.append(valuePlaceHolder);
						addParameterColumnAndValue(sqlCol, sapValue);
					}
					else
						bldr.append(sqlCol.sapelliObjectToLiteral(sapValue, true));
				}
				else
				{	// Null comparisons: see class javadoc
					bldr.append("IS");
					if(!equalityConstr.isEqual())
						bldr.append("NOT");
					bldr.append(getNullString()); // "NULL"
				}
			}
			else if(cp.getColumn() instanceof RecordColumn<?> && /* just to be sure: */ equalityConstr.getValue() instanceof Record)
			{	// Equality constraint on composite column...
				List<SColumn> subSqlCols = table.getSQLColumns((RecordColumn<?>) cp.getColumn());
				if(subSqlCols != null)
				{	// ...  which is split up in the SQLTable...
					Record valueRecord = (Record) equalityConstr.getValue();
					AndConstraint andConstr = new AndConstraint();
					for(SColumn subSqlCol : subSqlCols)
						andConstr.addConstraint(new EqualityConstraint(subSqlCol.sourceColumnPointer, subSqlCol.sourceColumnPointer.retrieveValue(valueRecord)));
					andConstr.reduce().accept(this);
				}
			}
			else
				exception = new DBException("Failed to generate SQL for equalityConstraint on column " + equalityConstr.getColumnPointer().getQualifiedColumnName(table.schema));
		}

		@Override
		public void visit(RuleConstraint ruleConstr)
		{
			// Check for null comparison (see class javadoc):
			if(ruleConstr.isRHSValue() && ruleConstr.getRHSValue() == null && (ruleConstr.getComparison() == Comparison.EQUAL || ruleConstr.getComparison() == Comparison.NOT_EQUAL)) // RuleConstraint only accepts null values in combination with in/equality comparison, so we don't need to check other cases
			{
				new EqualityConstraint(ruleConstr.getLHSColumnPointer(), null, ruleConstr.getComparison() == Comparison.EQUAL).accept(this);
				return;
			}
			// All other cases:
			SColumn lhsSCol = table.getSQLColumn(ruleConstr.getLHSColumnPointer());
			bldr.append(lhsSCol.name);
			bldr.append(getComparisonOperator(ruleConstr.getComparison()));
			if(ruleConstr.isRHSColumn())
				bldr.append(table.getSQLColumn(ruleConstr.getRHSColumnPointer()).name);
			else
			{
				Object sapValue = ruleConstr.getRHSValue();
				if(isParameterised())
				{
					bldr.append(valuePlaceHolder);
					addParameterColumnAndValue(lhsSCol, sapValue);
				}
				else
					bldr.append(lhsSCol.sapelliObjectToLiteral(sapValue, true));
			}
		}
		
	}
	
	/**
	 * Helper class to build SELECT queries (parameterised or literal)
	 * 
	 * @author mstevens
	 */
	protected class RecordSelectHelper extends RecordsByConstraintsHelper
	{
		
		/**
		 * @param table
		 * @param recordQuery
		 */
		public RecordSelectHelper(STable table, RecordsQuery recordsQuery)
		{
			super(table);
			
			// Build SELECT query:
			buildQuery(recordsQuery, "*"); // * = select all columns
			bldr.append(";", false);
		}

		/**
		 * @param table
		 * @param extremeValueRecordQuery
		 */
		public RecordSelectHelper(STable table, ExtremeValueRecordQuery extremeValueRecordQuery)
		{
			super(table);
			
			SColumn extremeValueSqlCol = table.getSQLColumn(extremeValueRecordQuery.getColumnPointer());
			if(extremeValueSqlCol == null)
			{
				exception = new DBException("Failed to generate SQL for extremeValueRecordQuery on column " + extremeValueRecordQuery.getColumnPointer().getQualifiedColumnName(table.schema));
				return;
			}
			
			// Build outer query:
			bldr.append("SELECT * FROM");
			bldr.append(table.tableName);
			bldr.append("WHERE");
			bldr.append(extremeValueSqlCol.name);
			bldr.append(getComparisonOperator(Comparison.EQUAL));
			bldr.append("(");
			bldr.openTransaction(SPACE);
			// Build subquery:
			buildQuery(extremeValueRecordQuery.getRecordsQuery(), (extremeValueRecordQuery.isMax() ? "MAX" : "MIN") + "(" + extremeValueSqlCol.name + ")");
			// Complete outer query:
			bldr.commitTransaction(false);
			bldr.append(")", false);
			bldr.append("LIMIT 1;");
		}
		
		/**
		 * @param table
		 */
		protected RecordSelectHelper(STable table)
		{
			super(table);
		}
		
		protected void buildQuery(RecordsQuery recordsQuery, String projection)
		{
			// Build query:
			bldr.append("SELECT");
			bldr.append(projection);
			bldr.append("FROM");
			bldr.append(table.tableName);
			// if there is not recordsQuery we are done here:
			if(recordsQuery == null)
				return;
			//else:
			// 	WHERE
			appendWhereClause(recordsQuery);
			// 	GROUP BY
			//		not supported (for now)
			//	ORDER BY
			Order order = recordsQuery.getOrder();
			if(order.isDefined())
			{
				bldr.append("ORDER BY");
				bldr.append(table.getSQLColumn(order.getBy()).name);
				bldr.append(order.isAsc() ? "ASC" : "DESC");
			}
			//	LIMIT
			if(recordsQuery.isLimited())
			{
				bldr.append("LIMIT");
				bldr.append(Integer.toString(recordsQuery.getLimit()));
			}
		}

	}
	
	/**
	 * @author mstevens
	 *
	 */
	protected class RecordCountHelper extends RecordSelectHelper
	{

		/**
		 * @param table
		 */
		public RecordCountHelper(STable table)
		{
			this(table, null);
		}
		
		/**
		 * @param table
		 * @param recordsQuery
		 */
		public RecordCountHelper(STable table, RecordsQuery recordsQuery)
		{
			super(table);
			
			// Build SELECT query:
			buildQuery(recordsQuery, "COUNT(*)");
			bldr.append(";", false);
		}
		
	}

	/**
	 * Helper class to build DELETE statements (parameterised or literal) for multiple records
	 * 
	 * @author mstevens
	 */
	protected class RecordsDeleteHelper extends RecordsByConstraintsHelper
	{
	
		/**
		 * @param table
		 * @param recordQuery
		 */
		public RecordsDeleteHelper(STable table, RecordsQuery recordsQuery)
		{
			// Initialise
			super(table);
			
			// Build statement:			
			bldr.append("DELETE FROM");
			bldr.append(table.tableName);
			// WHERE clause:
			appendWhereClause(recordsQuery);
			bldr.append(";", false);
		}
		
	}

}
