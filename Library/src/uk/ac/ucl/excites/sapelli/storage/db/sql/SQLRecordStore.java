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
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBConstraintException;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBPrimaryKeyException;
import uk.ac.ucl.excites.sapelli.shared.util.CollectionUtils;
import uk.ac.ucl.excites.sapelli.shared.util.TransactionalStringBuilder;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.StorageClient.RecordOperation;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.ColumnSet;
import uk.ac.ucl.excites.sapelli.storage.model.ListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Model;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.RecordReference;
import uk.ac.ucl.excites.sapelli.storage.model.RecordValueSet;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSet;
import uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn;
import uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ByteArrayListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.indexes.Index;
import uk.ac.ucl.excites.sapelli.storage.queries.ExtremeValueRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.FirstRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.Order;
import uk.ac.ucl.excites.sapelli.storage.queries.Query;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery;
import uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery.Executor;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.AndConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.BitFlagConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.Constraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.ConstraintVisitor;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.EqualityConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.JoinConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.NotConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.OrConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.RuleConstraint;
import uk.ac.ucl.excites.sapelli.storage.queries.constraints.RuleConstraint.Comparison;
import uk.ac.ucl.excites.sapelli.storage.queries.sources.Source;
import uk.ac.ucl.excites.sapelli.storage.queries.sources.SourceByFlags;
import uk.ac.ucl.excites.sapelli.storage.queries.sources.SourceBySet;
import uk.ac.ucl.excites.sapelli.storage.queries.sources.SourceResolver;
import uk.ac.ucl.excites.sapelli.storage.types.LineColumn;
import uk.ac.ucl.excites.sapelli.storage.types.LocationColumn;
import uk.ac.ucl.excites.sapelli.storage.types.OrientationColumn;
import uk.ac.ucl.excites.sapelli.storage.types.PolygonColumn;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;
import uk.ac.ucl.excites.sapelli.storage.visitors.SchemaTraverser;

/**
 * Abstract {@link RecordStore} implementation which is backed by an SQL-based relational database.
 * 
 * @author mstevens
 *
 * @param <SRS>
 * @param <STable>
 * @param <SColumn>
 */
public abstract class SQLRecordStore<SRS extends SQLRecordStore<SRS, STable, SColumn>, STable extends SQLRecordStore<SRS, STable, SColumn>.SQLTable, SColumn extends SQLRecordStore<SRS, STable, SColumn>.SQLColumn<?, ?>> extends RecordStore
{
	
	// STATIC ------------------------------------------------------------
	static protected final String SPACE = " ";
	
	// DYNAMIC -----------------------------------------------------------
	private STable modelsTable;
	private STable schemataTable;
	private InitArguments initArgs;
	
	/**
	 * Helper for {@link #retrieveRecords(RecordsQuery)}.
	 */
	private SelectRunner<Record, STable> recordSelectRunner = new SelectRunner<Record, STable>()
	{
		@Override
		public List<Record> run(STable table, RecordsQuery query) throws DBException
		{
			return table.select(query);
		}
	};
	
	/**
	 * Helper for {@link #retrieveRecordReferences(RecordsQuery)}.
	 */
	private SelectRunner<RecordReference, STable> recordReferenceSelectRunner = new SelectRunner<RecordReference, STable>()
	{
		@Override
		public List<RecordReference> run(STable table, RecordsQuery query) throws DBException
		{
			return table.selectReferences(query);
		}
	};
	
	/**
	 * Maps references to(!) "schemaMetaRecords" (records of Schema.META_SCHEMA, each describing a Schema) to the table corresponding to the described Schema
	 */
	private final Map<RecordReference, STable> tables;
	
	/**
	 * If non-null (all) SQL statements/queries will use parameters instead of literal values
	 */
	private final String valuePlaceHolder;
	
	/**
	 * The names of tables that are protected.
	 */
	private final Set<String> protectedTables = new HashSet<String>();
	
	/**
	 * @param client
	 * @param valuePlaceHolder - may be null if no parameters are to be used on (all) SQL statements/queries (only literal values)
	 */
	public SQLRecordStore(StorageClient client, String valuePlaceHolder)
	{
		super(client, true); // make use of roll-back tasks
		this.tables = new HashMap<RecordReference, STable>();
		this.valuePlaceHolder = valuePlaceHolder;
		
		// Protected tables:
		this.protectedTables.add(sanitiseIdentifier(Model.MODEL_SCHEMA.tableName));
		this.protectedTables.add(sanitiseIdentifier(Model.SCHEMA_SCHEMA.tableName));
		// subclasses can add additional protected tables during initialisation
	}
	
	/**
	 * @param initArgs the initInfo to set
	 */
	protected final void setInitialisationArguments(boolean newDB, int targetVersion, SQLRecordStoreUpgrader upgrader)
	{
		this.initArgs = new InitArguments(newDB, targetVersion, upgrader);
	}

	protected final void addProtectedTable(String tableName) throws DBException
	{
		if(!isInitialising())
			throw new DBException("Protected tables can only be added during initialisation");
		//else:
		if(tableName != null && !tableName.isEmpty())
			protectedTables.add(tableName);
		else
			throw new DBException("Protected table name cannot be null or empty!");
	}
	
	/**
	 * @author mstevens
	 */
	private final class InitArguments
	{
		
		public final boolean newDB;
		public final int targetVersion;
		public final SQLRecordStoreUpgrader upgrader;
		
		/**
		 * @param newDB
		 * @param targetVersion the version we want to database to be in or be upgrade to
		 * @param upgrader
		 */
		public InitArguments(boolean newDB, int targetVersion, SQLRecordStoreUpgrader upgrader)
		{
			this.newDB = newDB;
			this.targetVersion = targetVersion;
			this.upgrader = upgrader;
		}
		
	}
	
	/**
	 * Subclasses may override this but *must* call super implementation.
	 * TODO somehow force the super call using annotations?
	 * 
	 * @see uk.ac.ucl.excites.sapelli.shared.db.Store#doInitialise()
	 */
	protected void doInitialise() throws DBException
	{
		if(initArgs == null)
			throw new DBException("Cannot initialise " + getClass().getSimpleName() + " because initialisation arguments have not been set!");
		try
		{
			// Create the Models and Schemata tables if they doesn't exist yet (i.e. for a new database):
			if(initArgs.newDB)
				startTransaction();
			this.modelsTable = getTable(Model.MODEL_SCHEMA, initArgs.newDB);
			this.schemataTable = getTable(Model.SCHEMA_SCHEMA, initArgs.newDB);
			if(initArgs.newDB)
				commitTransaction();
			
			// Database version matters:
			if(!initArgs.newDB)
			{
				// Upgrade existing database if necessary:
				if(getVersion() < initArgs.targetVersion && initArgs.upgrader != null)
					initArgs.upgrader.upgrade(this, initArgs.targetVersion); // will set the new version if successful
			}
			else
				// Set version on new database:
				setVersion(initArgs.targetVersion);
		}
		catch(DBException e)
		{
			try
			{
				rollbackTransactions();
			}
			catch(Exception ignore) {}
			throw e;
		}
		finally
		{
			initArgs = null;
		}
	}

	/**
	 * Gets the current version of the database.
	 * 
	 * @throws DBException 
	 */
	public abstract int getVersion() throws DBException;
	
	/**
	 * Sets the current version of the database (persistently stored).
	 * 
	 * @param version
	 * @throws DBException
	 */
	protected abstract void setVersion(int version) throws DBException;
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#isStorable(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	public boolean isStorable(Record record)
	{
		return super.isStorable(record, isInitialising()); // allow storing of meta records only during initialisation/upgrade
	}
	
	protected abstract void executeSQL(String sql) throws DBException;
	
	protected abstract int executeSQLReturnAffectedRows(String sql) throws DBException;
	
	protected abstract String sanitiseIdentifier(String identifier);
	
	/**
	 * Checks whether a table for the given schema exists in the database.
	 * 
	 * @param schema
	 * @return
	 */
	protected final boolean doesTableExist(Schema schema)
	{
		return doesTableExist(sanitiseIdentifier(schema.tableName));
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
	protected final STable getTable(Schema schema, boolean createWhenNotInDB) throws DBException
	{
		return getTable(schema, createWhenNotInDB, isInitialising()); // don't allow access to modelsTable & schemataTable unless we are initialising
	}
	
	/**
	 * @param schema
	 * @param createWhenNotInDB
	 * @param allowMeta whether or not to provide access to the modelsTable & schemataTable
	 * @return
	 * @throws DBException
	 */
	protected final STable getTable(Schema schema, boolean createWhenNotInDB, boolean allowMeta) throws DBException
	{
		// Check is we are ready:
		if(!isInitialised() && !isInitialising())
			throw new DBException(getClass().getSimpleName() + " is not initialised!");
		// Check known tables:
		STable table = null;
		RecordReference schemaMetaRecordRef = null;
		if(Model.META_MODEL.contains(schema))
		{
			if(!allowMeta)
				throw new DBException("Direct manipulation of modelsTable or schemataTable is not allowed!");
			//else:
			if(schema == Model.MODEL_SCHEMA)
				table = modelsTable; // may still be null if getTable() was called from initialise()
			else if(schema == Model.SCHEMA_SCHEMA)
				table = schemataTable; // may still be null if getTable() was called from initialise()
		}
		else
		{
			schemaMetaRecordRef = schema.getMetaRecordReference(); // get reference to schemaMetaRecord
			table = tables.get(schemaMetaRecordRef); // lookup in tables cache
		}
		
		// If not found, generate new SQLTable object for the Schema:
		if(table == null)
		{
			table = getTableFactory().generateTable(schema);
			if(!Model.META_MODEL.contains(schema)) // the "tables" map is only for tables of "real" (non-meta) schemata!
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
				if(!Model.META_MODEL.contains(schema))
				{
					if(!modelsTable.isRecordInDB(Model.GetModelRecordReference(schema.getModel()))) // check if model is already known (due to one of its other schemata being present)
						modelsTable.insert(Model.GetModelRecord(schema.getModel(), client));
					schemataTable.insert(schema.getMetaRecord());
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
						if(doesTableExist(schema))
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
	
	protected Collection<Schema> getSchemata(Source source)
	{
		if(source.isNone())
			return Collections.<Schema> emptyList();
		else if(source.isAny())
			return getAllKnownSchemata();
		else
			return source.getSchemata(new SourceResolver()
			{
				@Override
				public Collection<Schema> resolve(SourceBySet sourceBySet)
				{
					return	sourceBySet.isByInclusion() ?
								sourceBySet.getSchemata() :
								getAllKnownSchemataExcept(sourceBySet.getSchemata());
				}

				@Override
				public Collection<Schema> resolve(SourceByFlags sourceByFlags)
				{
					return getKnownSchemata(new BitFlagConstraint(Model.SCHEMA_FLAGS_COLUMN, sourceByFlags.getFlags()));
					/*// Less efficient alternative:
					return sourceByFlags.filterSchemata(getAllKnownSchemata());*/
				}
			});
	}
	
	protected List<Schema> getAllKnownSchemata()
	{
		return getKnownSchemata(null);
	}
	
	protected List<Schema> getAllKnownSchemataExcept(Set<Schema> skipSchemata)
	{
		// Construct constraint to filter out undesired schemata:
		AndConstraint filterSkipSchemata = new AndConstraint();
		for(Schema skippedSchema : skipSchemata)
			filterSkipSchemata.addConstraint(Schema.GetMetaRecordReference(skippedSchema).getRecordQueryConstraint().negate());
		
		// Query for schemata, filtering out the undesired ones:
		return getKnownSchemata(filterSkipSchemata);
	}
	
	protected List<Schema> getKnownSchemata(Constraint constraint)
	{
		try
		{
			// Check if we know any schema at all:
			if(schemataTable == null || schemataTable.isEmpty())
				return Collections.<Schema> emptyList(); // we're done here
			
			// Query schemata table, using the given constraint:
			List<Record> schemaMetaRecords = schemataTable.select(new RecordsQuery(Source.From(Model.SCHEMA_SCHEMA), constraint));
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
					RecordReference modelRef = Model.SCHEMA_MODEL_ID_COLUMN.retrieveValue(schemaMetaRecord);
					// ... first check the model cache:
					Model model = modelCache.get(Model.MODEL_ID_COLUMN.retrieveValue(modelRef));
					if(model == null)
					{	// model is not in cache, so query the models table:
						model = Model.FromModelRecord(modelsTable.select(modelRef.getRecordQuery()), client); // model object obtained by deserialising model record
						modelCache.put(model.id, model); // remember model in cache
					}
					// Get the schema from the model object:
					schema = model.getSchema(Model.SCHEMA_SCHEMA_NUMBER_COLUMN.retrieveValue(schemaMetaRecord).intValue());
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
	 * @deprecated
	 */
	protected Schema getStoredVersion(Schema schema)
	{
		try
		{
			Record schemaMetaRecord = schema.getMetaRecord();
			RecordReference modelRef = Model.SCHEMA_MODEL_ID_COLUMN.retrieveValue(schemaMetaRecord);
			Model model = Model.FromModelRecord(modelsTable.select(modelRef.getRecordQuery()), client); // model object obtained by deserialising model record
			return model.getSchema(Model.SCHEMA_SCHEMA_NUMBER_COLUMN.retrieveValue(schemaMetaRecord).intValue());
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
			return null;
		}
	}
	
	protected abstract TableFactory<STable> getTableFactory();
	
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
			client.storageEvent(RecordOperation.Deleted, recordRef); // inform client
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
		for(Schema schema : getSchemata(query.getSource()))
		{
			try
			{
				STable table = getTable(schema, false);
				if(!table.isInDB())
					continue; // table does no exist in DB, so there are no records to retrieve
				if(!schema.hasFlags(StorageClient.SCHEMA_FLAG_TRACK_CHANGES))
					// Efficient but does not allow to report which records were deleted:
					table.delete(query);
				else
				{	// Less efficient, but allows to inform client:
					for(RecordReference recordRef : retrieveRecordReferences(new RecordsQuery(schema, query.getConstraints())))
						if(table.delete(recordRef))
							client.storageEvent(RecordOperation.Deleted, recordRef); // inform client
				}
			}
			catch(DBException dbE)
			{
				dbE.printStackTrace(System.err);
			}
		}
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#retrieveRecords(uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery)
	 */
	@Override
	public List<Record> retrieveRecords(RecordsQuery query)
	{
		return retrieveRecordValueSets(query, recordSelectRunner);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#retrieveRecordReferences(uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery)
	 */
	@Override
	public List<RecordReference> retrieveRecordReferences(RecordsQuery query)
	{
		return retrieveRecordValueSets(query, recordReferenceSelectRunner);
	}
	
	/**
	 * @author mstevens
	 *
	 * @param <R>
	 */
	private interface SelectRunner<R extends RecordValueSet<?>, STable>
	{
		
		public List<R> run(STable table, RecordsQuery query) throws DBException;
		
	}
	
	private <R extends RecordValueSet<?>> List<R> retrieveRecordValueSets(RecordsQuery query, SelectRunner<R, STable> selectRunner)
	{
		List<R> resultAcc = null;
		// Run subqueries for each schema in the query, or all known schemata (if the query is for "any" schema):
		for(Schema s : getSchemata(query.getSource()))
		{
			try
			{
				STable table = getTable(s, false);
				if(!table.isInDB())
					continue; // table does no exist in DB, so there are no records to retrieve
				List<R> subResult = selectRunner.run(table, query);
				if(!subResult.isEmpty())
				{
					if(resultAcc == null)
						resultAcc = new ArrayList<R>(subResult.size());
					resultAcc.addAll(subResult);
				}
			}
			catch(DBException dbE)
			{
				dbE.printStackTrace(System.err);
			}
		}
		return resultAcc != null ? resultAcc : Collections.<R> emptyList();
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
	 * To be overridden by subclasses.
	 * 
	 * @return
	 */
	public final Set<String> getProtectedTableNames()
	{
		return Collections.unmodifiableSet(protectedTables);
	}
	
	/**
	 * @return a {@link List} of the names of all tables in the database
	 */
	protected abstract List<String> getAllTableNames();
	
	/**
	 * Drops the table with the given name. Use with care!
	 * Will fail, with {@link DBException} thrown, if the table is protected and unless {@code force} is {@code true}.
	 * 
	 * @param tableName
	 * @param force
	 * @throws DBException
	 */
	protected void dropTable(String tableName, boolean force) throws DBException
	{
		if(!doesTableExist(tableName))
		{	// Try sanitising:
			tableName = sanitiseIdentifier(tableName);
			if(!doesTableExist(tableName))
				return; // there is no such table
		}
		//else:
		if(!protectedTables.contains(tableName) || force)
		{
			// Get table instance:
			STable table = null;
			if(modelsTable.tableName.equals(tableName))
				table = modelsTable;
			else if(schemataTable.tableName.equals(tableName))
				table = schemataTable;
			else
			{
				// Delete schemata entry:
				if(schemataTable.isInDB())
					schemataTable.delete(new RecordsQuery(Model.SCHEMA_SCHEMA, new EqualityConstraint(Model.SCHEMA_TABLE_NAME_COLUMN, tableName)));
				
				// Look for table in tables map:
				RecordReference schemaRecRef = null;
				for(Map.Entry<RecordReference, STable> entry : tables.entrySet())
					if(entry.getValue().tableName.equals(tableName))
					{
						schemaRecRef = entry.getKey();
						table = entry.getValue();
						break;
					}
				
				// Delete from tables map:
				if(schemaRecRef != null)
					tables.remove(schemaRecRef);
			}
			
			// Release resources so we can drop:
			release();
			
			// DROP table:
			if(table != null)
				table.drop();
			else
				executeSQL(generateDropTableStatement(tableName));
		}
		else
			throw new DBException("Cannot delete protected table '" + tableName + "'!");
	}
	
	protected String generateDropTableStatement(String tableName)
	{
		return String.format("DROP TABLE %s;", tableName);
	}
	
	/**
	 * Renames the table with the given old name to the given new name. Use with care!
	 * Will fail, with {@link DBException} thrown, if the table is protected.
	 * 
	 * @param oldTableName
	 * @param newTableName
	 * @throws DBException
	 */
	protected void renameTable(String oldTableName, String newTableName) throws DBException
	{
		if(!doesTableExist(oldTableName))
		{	// Try sanitising:
			oldTableName = sanitiseIdentifier(oldTableName);
			if(!doesTableExist(oldTableName))
				return; // there is no such table
		}
		//else:
		if(!protectedTables.contains(oldTableName))
		{
			// Sanitise:
			newTableName = sanitiseIdentifier(newTableName);
			
			// Rename database table:
			executeSQL(String.format("ALTER TABLE %1$s RENAME TO %2$s;", oldTableName, newTableName));
			
			// Query schemata table:
			Record schemaMetaRecord = schemataTable.select(new FirstRecordQuery(new RecordsQuery(Model.SCHEMA_SCHEMA, new EqualityConstraint(Model.SCHEMA_TABLE_NAME_COLUMN, oldTableName))));
			if(schemaMetaRecord != null)
			{
				// Update schemata entry:
				Model.SCHEMA_TABLE_NAME_COLUMN.storeValue(schemaMetaRecord, newTableName);
				schemataTable.update(schemaMetaRecord);
			
				// Delete table from tables map:
				tables.remove(schemaMetaRecord.getReference()); // new STable will be constructed & added to the tables map when the renamed table is first accessed
			}
		}
		else
			throw new DBException("Cannot rename protected table '" + oldTableName + "'!");
	}
	
	/**
	 * Release any open resources associated with the database connection (without closing it).
	 * 
	 * Subclasses may override this but *must* call super implementation.
	 * TODO somehow force the super call using annotations?
	 */
	protected void release()
	{
		// Release resources for all tables:
		if(modelsTable != null)
			modelsTable.release();
		if(schemataTable != null)
			schemataTable.release();
		for(STable table : tables.values())
			table.release();
	}
	
	@Override
	protected void doClose() throws DBException
	{
		release();
		
		super.doClose(); // !!!
	}
	
	/**
	 * @author mstevens
	 *
	 */
	public abstract class SQLTable
	{

		/**
		 * Sanitised version of schema.tableName.
		 */
		public final String tableName;
		
		public final Schema schema;
		
		@SuppressWarnings("unchecked")
		protected final RecordSelectionProjection recordSelectionProjection = new RecordSelectionProjection((STable) this);
		
		@SuppressWarnings("unchecked")
		protected final RecordReferenceSelectionProjection recordReferenceSelectionProjection = new RecordReferenceSelectionProjection((STable) this);
		
		private Boolean existsInDB;
		private TableCreationHelper creator;
		
		/**
		 * Mapping of Sapelli ColumnPointers (usually leaf columns) to corresponding  SQLColumns.
		 */
		public final Map<ColumnPointer<?>, SColumn> sqlColumns;
		
		/**
		 * Mapping of composite Sapelli columns to a list of SQLColumns which they correspond to.
		 * E.g. Location --> (Lat, Lon, ...) 
		 */
		public final Map<ValueSetColumn<?, ?>, List<SColumn>> composite2SqlColumns;
		
		/**
		 * The auto-incrementing primary key column as (a Sapelli Column, not an S/SQLColumn), will be null if there is none
		 */
		protected final IntegerColumn autoIncrementKeySapColumn;
		protected SColumn autoIncrementKeySQLColumn;
		
		/**
		 * Contains the SQLColumns that correspond to (parts of) the Schema's public key
		 */
		private Set<SColumn> keyPartSqlColumns;
		
		public SQLTable(Schema schema)
		{
			this.tableName = sanitiseIdentifier(schema.tableName);
			this.schema = schema;
			// Init collections:
			sqlColumns = new LinkedHashMap<ColumnPointer<?>, SColumn>(); // to preserve column order we use a LinkedHashMap (i.e. a collection that is iterated in insertion-order)!
			composite2SqlColumns = new HashMap<ValueSetColumn<?, ?>, List<SColumn>>();
			// Deal with auto-increment key:
			this.autoIncrementKeySapColumn = schema.getAutoIncrementingPrimaryKeyColumn();
		}
		
		/**
		 * @param sqlColumn
		 */
		public void addColumn(SColumn sqlColumn)
		{
			if(existsInDB != null)
				throw new IllegalStateException("Cannot add columns to SQLTable that exists in the database or whose's creation has been attempted.");
				
			ColumnPointer<?> sourceCP = sqlColumn.sourceColumnPointer;
			if(sourceCP == null || sourceCP.getColumn() == null)
				throw new IllegalArgumentException("SQLColumn needs a valid sourceColumnPointer in order to be added to a SQLTable instance");
			
			// Add SQLColumn:
			if(sqlColumns.get(sourceCP) != null)
				throw new IllegalArgumentException("Duplicate source column!");
			sqlColumns.put(sourceCP, sqlColumn);
			
			// Deal with AutoIncr...
			if(sourceCP.getColumn() == autoIncrementKeySapColumn)
				this.autoIncrementKeySQLColumn = sqlColumn;
			
			// Deal with composites...
			while(sourceCP.isSubColumn())
			{
				ColumnPointer<ValueSetColumn<?, ?>> parentCP = sourceCP.getParentPointer();
				List<SColumn> subSQLCols;
				if(composite2SqlColumns.containsKey(parentCP.getColumn()))
					subSQLCols = composite2SqlColumns.get(parentCP.getColumn());
				else
				{
					subSQLCols = new ArrayList<SColumn>();
					composite2SqlColumns.put(parentCP.getColumn(), subSQLCols);
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
		
		public SColumn getSQLColumn(ColumnPointer<?> sapColumnPointer)
		{
			// Try pointer as such (assumes it contains a complete path):
			SColumn sqlCol = sqlColumns.get(sapColumnPointer);
			if(sqlCol == null)
				// Try to find the column:
				sqlCol = getSQLColumn(sapColumnPointer.getColumn());
			return sqlCol;
		}
		
		public <C extends Column<?>> SColumn getSQLColumn(C sapColumn)
		{
			return sqlColumns.get(new ColumnPointer<C>(schema, sapColumn));
		}
		
		public List<SColumn> getSQLColumns(Column<?> sapColumn)
		{
			if(sapColumn instanceof ValueSetColumn)
				return composite2SqlColumns.get((ValueSetColumn<?, ?>) sapColumn);
			else
				return Collections.singletonList(getSQLColumn(sapColumn));
		}
		
		public Set<SColumn> getKeyPartSQLColumns()
		{
			if(keyPartSqlColumns == null)
			{
				keyPartSqlColumns = new LinkedHashSet<SColumn>(); // to preserve column order we use a LinkedHashSet (i.e. a collection that is iterated in insertion-order)! 
				for(Column<?> sapKeyPartCol : schema.getPrimaryKey().getColumns(false))
					// sapKeyPartCol may be a composite (like a ForeignKeyColumn), so loop over each SColumn that represents part of it:
					for(SColumn sqlKeyPartCol : getSQLColumns(sapKeyPartCol))
						CollectionUtils.addIgnoreNull(keyPartSqlColumns, sqlKeyPartCol);
			}
			return keyPartSqlColumns;
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
		 * Returns the currently stored version of the given Record or indicated by the given RecordReference.
		 * 
		 * @param recordOrReference
		 * @return
		 * @throws NullPointerException
		 * @throws DBException
		 */
		public Record getStoredVersion(RecordValueSet<?> recordOrReference) throws DBException
		{
			if(isInDB() && recordOrReference.isReferenceable() /*also checks autoIncrPK*/)
				return select(recordOrReference.getRecordQuery());
			else
				return null;
		}
		
		/**
		 * Checks if the given {@link Record}, or the one indicated by the given {@link RecordReference}, already exists in the database table.
		 * 
		 * May be overridden.
		 * 
		 * @param recordOrReference
		 * @return
		 * @throws NullPointerException
		 * @throws DBException
		 */
		public boolean isRecordInDB(RecordValueSet<?> recordOrReference) throws DBException
		{
			return getStoredVersion(recordOrReference) != null;
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
		 * Delete existing record (given as a Record or RecordReference) in database table.
		 * Assumes the table exists in the database!
		 * 
		 * May be overridden.
		 * 
		 * @param recordOrReference a {@link RecordValueSet} instance, either the {@link Record} itself or a {@link RecordReference} pointing to it
		 * @return whether the record was really deleted
		 * @throws DBException
		 */
		@SuppressWarnings("unchecked")
		public boolean delete(RecordValueSet<?> recordOrReference) throws DBException
		{
			return executeSQLReturnAffectedRows(new RecordDeleteHelper((STable) this, recordOrReference).getQuery()) == 1;
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
		 * Selects {@link Record}s from the database table based on a {@link RecordsQuery}.
		 * Assumes the table exists in the database!
		 * 
		 * @param query
		 * @return a {@link List} of {@link Record}s, possibly empty, never {@code null}
		 * @throws DBException
		 */
		@SuppressWarnings("unchecked")
		public List<Record> select(RecordsQuery query) throws DBException
		{
			return executeRecordSelection(new RecordValueSetSelectHelper<Record>((STable) this, recordSelectionProjection, query));
		}
		
		/**
		 * Selects {@link RecordReference}s from the database table based on a {@link RecordsQuery}.
		 * Assumes the table exists in the database!
		 * 
		 * @param query
		 * @return a {@link List} of {@link RecordReference}s, possibly empty, never {@code null}
		 * @throws DBException
		 */
		@SuppressWarnings("unchecked")
		public List<RecordReference> selectReferences(RecordsQuery query) throws DBException
		{
			return executeRecordSelection(new RecordValueSetSelectHelper<RecordReference>((STable) this, recordReferenceSelectionProjection, query));
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
					return executeRecordSelection(new ExtremeValueRecordSelectHelper((STable) SQLTable.this, extremeValueRecordQuery)); 
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
			executeSQL(generateDropTableStatement(tableName));
			// Note: if there is an exception the line below will not be executed but the roll-back task above will...
			
			// Now the table is gone...
			existsInDB = false; // !!!
		}
		
		/**
		 * @param recordValueSetSelectHelper
		 * @return a {@link List} of {@link RecordValueSet}s (i.e. {@link Record}s or {@link RecordReference}s), possibly empty, never {@code null}
		 * @throws DBException
		 */
		protected abstract <R extends RecordValueSet<?>> List<R> executeRecordSelection(RecordValueSetSelectHelper<R> recordValueSetSelectHelper) throws DBException;
		
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
		public final ColumnPointer<? extends Column<SapType>> sourceColumnPointer;
		protected final TypeMapping<SQLType, SapType> mapping;

		/**
		 * @param name may be null only if sourceColumnPointer is not
		 * @param type
		 * @param sourceColumnPointer - may be null in specific hackish cases (e.g. {@link SQLiteRecordStore#doesTableExist(String)}) and on the condition that name is not null
		 * @param mapping - may be null in case SQLType = SapType
		 */
		@SuppressWarnings("unchecked")
		public SQLColumn(String name, String type, ColumnPointer<? extends Column<SapType>> sourceColumnPointer, TypeMapping<SQLType, SapType> mapping)
		{
			this.sourceColumnPointer = sourceColumnPointer;
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
		 * @param recordOrReference
		 * @param quotedIfNeeded
		 * @return
		 */
		public String retrieveAsLiteral(RecordValueSet<?> recordOrReference, boolean quotedIfNeeded)
		{
			return sqlToLiteral(retrieve(recordOrReference), quotedIfNeeded);
		}
		
		/**
		 * @param recordOrReference
		 * @return
		 */
		@SuppressWarnings("unchecked")
		public SQLType retrieve(RecordValueSet<?> recordOrReference)
		{
			SapType value = (SapType) sourceColumnPointer.retrieveValue(recordOrReference);
			return value != null ? mapping.toSQLType(value) : null;
		}
		
		/**
		 * @param recordOrReference
		 * @param value
		 */
		public void store(RecordValueSet<?> recordOrReference, SQLType value)
		{
			// Get column:
			Column<SapType> col = sourceColumnPointer.getColumn();
			// Get valueSet (i.e. the (sub)record):
			ValueSet<?> valueSet = sourceColumnPointer.getValueSet(recordOrReference, value != null); // only create if we have a non-null value to set
			// Set or clear value:
			if(value != null)
				col.storeValue(valueSet, mapping.toSapelliType(value));
			else if(valueSet != null) // only clear if we have a non-null valueSet:
				col.clearValue(valueSet);
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
		
		public boolean isSapelliColumn()
		{
			return sourceColumnPointer != null;
		}
		
		public boolean isBoolColForAllOptionalValueSetCol()
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
	public interface TableFactory<STable>
	{
		
		/**
		 * Generate a SQLTable for a Schema
		 * 
		 * @param schema
		 * @return
		 * @throws DBException
		 */
		public STable generateTable(Schema schema) throws DBException;
		
		/**
		 * @param enable if {@code true} the TableFactory will insert a top-level Boolean column to represent a ValueSetColumn with all-optional subcolumns
		 * @throws DBException 
		 */
		public void setInsertBoolColsForAllOptionalValueSetCols(boolean enable) throws DBException;
		
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
	public abstract class BasicTableFactory extends SchemaTraverser implements TableFactory<STable>
	{
		
		protected STable table;
		
		private boolean insertBoolColsForAllOptionalValueSetCols = true; // !!!
		
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
		
		@Override
		public void setInsertBoolColsForAllOptionalValueSetCols(boolean enable) throws DBException
		{
			if(!isInitialising())
				throw new DBException("Changing 'insertBoolColsForAllOptionalValueSetCols' is only allowed during initialisation/upgrade!");
			insertBoolColsForAllOptionalValueSetCols = enable;
		}
		
		/* (non-Javadoc)
		 * @see uk.ac.ucl.excites.sapelli.storage.visitors.SchemaTraverser#enter(uk.ac.ucl.excites.sapelli.storage.model.ValueSetColumn)
		 */
		@Override
		public <VS extends ValueSet<CS>, CS extends ColumnSet> void enter(final ValueSetColumn<VS, CS> valueSetCol)
		{
			if(insertBoolColsForAllOptionalValueSetCols && valueSetCol.hasAllOptionalSubColumns())
			{	// Insert Boolean column to enable us to differentiate between a ValueSet that is null or one that has null values in all its subcolumns:
				addBoolColForAllOptionalValueSetCol(
					getColumnPointer(valueSetCol),
					new TypeMapping<Boolean, VS>()
					{
						@Override
						public Boolean toSQLType(VS value)
						{
							return value != null ? Boolean.TRUE : null;
						}
		
						@Override
						public VS toSapelliType(Boolean value)
						{
							if(value != null && value.booleanValue())
								return valueSetCol.getNewValueSet();
							else
								return null;
						}
					});
			}
			// !!!
			super.enter(valueSetCol);
		}

		/**
		 * Must add a SQLColumn<Boolean, VS> to the table and this column instance must return {@code true} for method {@link SQLColumn#isBoolColForAllOptionalValueSetCol()}.
		 * 
		 * @param sourceColumnPointer
		 * @param mapping
		 */
		protected abstract <VS extends ValueSet<CS>, CS extends ColumnSet> void addBoolColForAllOptionalValueSetCol(ColumnPointer<? extends Column<VS>> sourceColumnPointer, TypeMapping<Boolean, VS> mapping);
		
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
		public <T> void visit(ListColumn.Simple<T> simpleListCol)
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
		public void visit(StringListColumn stringListCol)
		{
			visitListColumn(stringListCol);
		}
		
		@Override
		public void visit(ByteArrayListColumn byteArrayListCol)
		{
			visitListColumn(byteArrayListCol);
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
		protected abstract String getColumnConstraint(ColumnPointer<?> sourceCP, List<Index> indexesToProcess);
		
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
		private String query;
		
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
			return getQuery(true); // close with ';' by default
		}
		
		public String getQuery(boolean close) throws DBException
		{
			if(exception != null)
				throw exception;
			if(bldr != null)
			{
				query = bldr.toString() + (close ? ";" : "");
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
			bldr.append(")", false);
		}
		
	}
	
	/**
	 * Helper class to build UPDATE statements (parameterised or literal)
	 * 
	 * @author mstevens
	 */
	protected class RecordUpdateHelper extends RecordsByConstraintsHelper
	{
		
		/**
		 * Parameterised
		 * 
		 * @param table
		 */
		public RecordUpdateHelper(STable table)
		{
			this(table, null /*indicates the query is parameterised*/);
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
				if(!table.getKeyPartSQLColumns().contains(sqlCol))
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
		}
		
	}
	
	/**
	 * Helper class to build DELETE statements (parameterised or literal) for single records
	 * 
	 * @author mstevens
	 */
	protected class RecordDeleteHelper extends RecordsByConstraintsHelper
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
		 * @param recordOrReference a {@link RecordValueSet} instance, either the {@link Record} itself or a {@link RecordReference} pointing to it
		 */
		public RecordDeleteHelper(STable table, RecordValueSet<?> recordOrReference)
		{
			// Initialise
			super(table);
			
			// Build statement:			
			bldr.append("DELETE FROM");
			bldr.append(table.tableName);
			// WHERE clause:
			appendWhereClause(recordOrReference);
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
	protected class RecordsByConstraintsHelper extends StatementHelper implements ConstraintVisitor
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
		
		/**
		 * Appends a WHERE clause which matches the PK (parts) of the given Record[Reference] (if non-null)
		 * or the PK columns of the Schema of the table (if the given Record[Reference] is null and the statement is parameterised).
		 * 
		 * @param a ValueSet<?> instance (when the statement is not parameterised) or null (when it is parameterised)
		 */
		protected void appendWhereClause(RecordValueSet<?> recordOrReference)
		{
			if(!isParameterised() && recordOrReference == null)
				throw new NullPointerException("recordOrReference cannot be null if statement is not parameterised");
			if(recordOrReference != null)
				appendWhereClause(recordOrReference.getRecordQueryConstraint());
			else
				appendWhereClause(table.schema.getBlankPKConstraints()); // only when parameterised!
		}
		
		/**
		 * Appends a WHERE clause matching the given constraint(s).
		 * 
		 * @param constraints
		 */
		protected void appendWhereClause(Constraint constraints)
		{
			if(constraints != null)
			{
				bldr.openTransaction();
				bldr.append("WHERE");
				bldr.openTransaction();
				constraints.accept(this); // start visiting of constraint(s)
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
			ColumnPointer<?> cp = equalityConstr.getColumnPointer();
			SColumn sqlCol = table.getSQLColumn(cp);
			Object sapValue = equalityConstr.getValue();
			if(sqlCol != null)
			{
				/* Note:
				 * 	sqlCol.isBoolColForAllOptionalValueSetCol() and table.getKeyPartSQLColumns().contains(sqlCol) are
				 * 	mutually exclusive because PrimaryKey does not allow any (sub)columns to be optional.
				 */
				if(sqlCol.isBoolColForAllOptionalValueSetCol() && sapValue != null)
				{	// Equality constraint (but not a null comparison) on composite column represented by a boolean SColumn:
					Constraint.Accept(new AndConstraint(EqualityConstraint.IsNotNull(cp), splitCompositeEquality(equalityConstr)).reduce(), this);
				}
				else
				{	// Equality constraint on non-composite (leaf) column (general case), or null comparison on a composite column represented by a boolean SColumn:
					bldr.append(sqlCol.name);
					if(sapValue != null || (table.getKeyPartSQLColumns().contains(sqlCol) && isParameterised()))
					{	// Value is not null, or null but part of the PK and this is a parameterised statement
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
					{	// Null comparisons (see class javadoc):
						bldr.append("IS");
						if(!equalityConstr.isEqual())
							bldr.append("NOT");
						bldr.append(getNullString()); // "NULL"
					}
				}
			}
			else if(cp.getColumn() instanceof ValueSetColumn<?, ?>)
			{	// Equality constraint on composite column (which is split up in the SQLTable):
				Constraint.Accept(splitCompositeEquality(equalityConstr), this);
			}
			else
				exception = new DBException("Failed to generate SQL for equalityConstraint on column " + equalityConstr.getColumnPointer().getQualifiedColumnName(table.schema));
		}
		
		private Constraint splitCompositeEquality(EqualityConstraint equalityConstr)
		{
			AndConstraint andConstr = new AndConstraint();
			List<SColumn> subSqlCols = table.getSQLColumns((ValueSetColumn<?, ?>) equalityConstr.getColumnPointer().getColumn());
			ValueSet<?> valueSet = (ValueSet<?>) equalityConstr.getValue();
			for(SColumn subSqlCol : subSqlCols)
					andConstr.addConstraint(new EqualityConstraint(subSqlCol.sourceColumnPointer, valueSet != null ? subSqlCol.sourceColumnPointer.retrieveValue(valueSet) : null));
			return andConstr.reduce(); 
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

		/**
		 * Produces: "(flagsColumn & flagsPatter) = flagsPattern"
		 * 
		 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.ConstraintVisitor#visit(uk.ac.ucl.excites.sapelli.storage.queries.constraints.BitFlagConstraint)
		 */
		@Override
		public void visit(BitFlagConstraint bitFlagConstr)
		{
			SColumn sqlCol = table.getSQLColumn(bitFlagConstr.getFlagsColumnPointer());
			bldr.append("(");
			bldr.append(sqlCol.name, false);
			bldr.append("&"); // bit-wise AND
			if(isParameterised())
			{
				bldr.append(valuePlaceHolder);
				addParameterColumnAndValue(sqlCol, bitFlagConstr.getFlagsPattern());
			}
			else
				bldr.append(sqlCol.sapelliObjectToLiteral(bitFlagConstr.getFlagsPattern(), true));
			bldr.append(")", false);
			bldr.append(getComparisonOperator(Comparison.EQUAL));
			if(isParameterised())
			{
				bldr.append(valuePlaceHolder);
				addParameterColumnAndValue(sqlCol, bitFlagConstr.getFlagsPattern());
			}
			else
				bldr.append(sqlCol.sapelliObjectToLiteral(bitFlagConstr.getFlagsPattern(), true));
		}

		/* (non-Javadoc)
		 * @see uk.ac.ucl.excites.sapelli.storage.queries.constraints.ConstraintVisitor#visit(uk.ac.ucl.excites.sapelli.storage.queries.constraints.JoinConstraint)
		 */
		@Override
		public void visit(JoinConstraint joinConstraint)
		{
			try
			{
				STable forgeinTable = getTable(joinConstraint.getForeignSchema(), false);
				// TODO what if column does exist? --> whole query should fail
				// TODO create another RecordsByConstraintsHelper instance for other table? and visit foreign constraints with it?
				// TODO al foreign column names must be prefixed
				
			}
			catch(DBException e)
			{
				exception = e;
			}
			
		}
		
	}
	
	/**
	 * Interface that provides the projection(String) for different kinds of SELECT queries, used by {@link SelectHelper}.
	 * 
	 * @author mstevens
	 */
	protected interface SelectProjection
	{
		
		/**
		 * Must return a String which specifies the "projection" part of a SQL SELECT query.
		 * For instance the "name, surname, dob" part from "SELECT name, surname, dob FROM Students;".
		 * 
		 * @return the projection String
		 */
		public String getProjectionString();
		
	}
	
	/**
	 * Helper class to build different kinds SELECT queries (parameterised or literal).
	 * 
	 * @author mstevens
	 *
	 * @param <SP> the {@link SelectProjection} type
	 */
	protected class SelectHelper<SP extends SelectProjection> extends RecordsByConstraintsHelper
	{
		
		public final SP projection;
		
		/**
		 * @param table
		 * @param projection
		 */
		public SelectHelper(STable table, SP projection)
		{
			this(table, projection, true);
		}
		
		/**
		 * @param table
		 * @param projection
		 * @param buildQueryNow whether or not to call {@link #buildQuery(RecordsQuery)} from this constructor
		 */
		protected SelectHelper(STable table, SP projection, boolean buildQueryNow)
		{
			this(table, projection, null, buildQueryNow);
		}
		
		/**
		 * @param table
		 * @param projection
		 * @param recordsQuery
		 */
		public SelectHelper(STable table, SP projection, Query<?> query)
		{
			this(table, projection, query, true);
		}
		
		/**
		 * @param table
		 * @param projection
		 * @param recordsQuery
		 * @param buildQueryNow whether or not to call {@link #buildQuery(RecordsQuery)} from this constructor
		 */
		protected SelectHelper(STable table, SP projection, Query<?> query, boolean buildQueryNow)
		{
			super(table);
			this.projection = projection;
			
			if(buildQueryNow)
				// Build SELECT query:
				buildQuery(query);
		}
		
		protected void buildQuery(Query<?> query)
		{
			// Build query:
			bldr.append("SELECT");
			bldr.append(projection.getProjectionString());
			bldr.append("FROM");
			bldr.append(table.tableName);
			// if there is no recordsQuery we are done here, unless forceWhereClause() returns true:
			if(query == null && !forceWhereClause())
				return;
			//else:
			// 	WHERE
			appendWhereClause(query != null ? query.getConstraints() : null);
			// if there is no recordsQuery we are *really* done here:
			if(query == null)
				return;
			//else:
			// 	GROUP BY
			//		not supported (for now)
			//	ORDER BY
			Order order = query.getOrder();
			if(order.isDefined())
			{
				bldr.append("ORDER BY");
				bldr.openTransaction(", ");
				// Loop over orderings:
				for(Order.Ordering ordering : order.getOrderings())
				{
					ColumnPointer<?> byCP = ordering.getBy();
					SColumn sqlCol = table.getSQLColumn(byCP);
					if(sqlCol != null)
					{
						// Column itself:
						addOrderBy(sqlCol, ordering.isAsc());
						// Special case...
						if(sqlCol.isBoolColForAllOptionalValueSetCol())
							// Order by each subcol as well:
							addCompositeOrderBy((ValueSetColumn<?, ?>) byCP.getColumn(), ordering.isAsc());
					}
					else if(byCP.getColumn() instanceof ValueSetColumn<?, ?>)
					{	// Ordering on composite column (which is split up in the SQLTable):
						addCompositeOrderBy((ValueSetColumn<?, ?>) byCP.getColumn(), ordering.isAsc());
					}
					else
						exception = new DBException("Failed to generate SQL for ORDER BY on column " + byCP.getQualifiedColumnName(table.schema));
				}
				bldr.commitTransaction();
			}
			//	LIMIT
			if(query.isLimited())
			{
				bldr.append("LIMIT");
				bldr.append(Integer.toString(query.getLimit()));
			}
		}
		
		private void addOrderBy(SColumn sqlCol, boolean asc)
		{
			bldr.openTransaction();
			bldr.append(sqlCol.name);
			bldr.append(asc ? "ASC" : "DESC");
			bldr.commitTransaction();
		}
		
		private void addCompositeOrderBy(ValueSetColumn<?, ?> valueSetCol, boolean asc)
		{
			for(SColumn subSqlCol : table.getSQLColumns(valueSetCol))
				addOrderBy(subSqlCol, asc);
		}
		
		/**
		 * Can be overridden with a method returning {@code true}, in which case {@link #appendWhereClause(Constraint)} will be called even when the {@link Query} is {@code null}.
		 * 
		 * @return
		 */
		protected boolean forceWhereClause()
		{
			return false;
		}

	}
	
	/**
	 * Class that provides the projection String and {@link SQLColumn}s for SELECT queries that result in {@link RecordValueSet}s (i.e. {@link Record}s or {@link RecordReference}s).
	 * 
	 * @author mstevens
	 *
	 * @param <R> the {@link RecordValueSet} type
	 */
	protected abstract class RecordValueSetSelectionProjection<R extends RecordValueSet<?>> implements SelectProjection
	{
		
		protected final STable table;
		
		public RecordValueSetSelectionProjection(STable table)
		{
			this.table = table;
		}
		
		/* (non-Javadoc)
		 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore.SelectProjection#getProjectionString()
		 */
		@Override
		public String getProjectionString()
		{
			// List all columns returned by getProjectionColumns():
			TransactionalStringBuilder projectionBldr = new TransactionalStringBuilder(", ");
			for(SColumn sqlCol : getProjectionColumns())
				projectionBldr.append(sqlCol.name);
			return projectionBldr.toString();
		}

		public abstract R createRecordValueSet();
		
		/**
		 * Must return the {@link SQLColumn}s that make up the SELECT projection. The returned {@link Collection}
		 * must have a defined (i.e. fixed) iteration order.
		 * If {@link #getProjectionString()} returns a listing of column names (e.g. "name, surname, dob") than
		 * the Collection returned here must contain the same columns (and only those) in the same order.
		 * If {@link #getProjectionString()} returns "*" (i.e. signifying all columns of the table) than the
		 * Collection returned here must contain all columns of the table (and only those) in the order in which
		 * they are placed in the table.
		 * 
		 * @return a {@link Collection} with the {@link SQLColumn}s that make up the projection
		 */
		public abstract Collection<SColumn> getProjectionColumns();
		
	}
	
	/**
	 * Helper class to build SELECT queries (parameterised or literal) that result in {@link RecordValueSet}s (i.e. {@link Record}s or {@link RecordReference}s).
	 * 
	 * @author mstevens
	 *
	 * @param <R> the {@link RecordValueSet} type
	 */
	protected class RecordValueSetSelectHelper<R extends RecordValueSet<?>> extends SelectHelper<RecordValueSetSelectionProjection<R>>
	{

		protected RecordValueSetSelectHelper(STable table, RecordValueSetSelectionProjection<R> projection, RecordsQuery recordsQuery)
		{
			this(table, projection, recordsQuery, true);
		}
		
		protected RecordValueSetSelectHelper(STable table, RecordValueSetSelectionProjection<R> projection, RecordsQuery recordsQuery, boolean buildQueryNow)
		{
			super(table, projection, recordsQuery, buildQueryNow);
		}
		
	}
	
	/**
	 * A {@link SelectProjection} class for the execution of SELECT queries that result in {@link Record}s.
	 * 
	 * @author mstevens
	 */
	protected class RecordSelectionProjection extends RecordValueSetSelectionProjection<Record>
	{

		public RecordSelectionProjection(STable table)
		{
			super(table);
		}

		@Override
		public String getProjectionString()
		{
			return "*"; // = all columns
		}

		@Override
		public Record createRecordValueSet()
		{
			return table.schema.createRecord();
		}

		@Override
		public Collection<SColumn> getProjectionColumns()
		{
			return table.sqlColumns.values();
		}
		
	}
	
	/**
	 * A {@link SelectProjection} class for the execution of SELECT queries that result in {@link RecordReference}s.
	 * 
	 * @author mstevens
	 */
	protected class RecordReferenceSelectionProjection extends RecordValueSetSelectionProjection<RecordReference>
	{

		public RecordReferenceSelectionProjection(STable table)
		{
			super(table);
		}

		@Override
		public RecordReference createRecordValueSet()
		{
			return table.schema.createRecordReference();
		}

		@Override
		public Collection<SColumn> getProjectionColumns()
		{
			return table.getKeyPartSQLColumns(); // only keyPartCols
		}
		
	}
	
	/**
	 * A {@link SelectHelper} class for the execution of SELECT COUNT(*) queries.
	 * 
	 * @author mstevens
	 */
	protected class RecordCountHelper extends SelectHelper<SelectProjection>
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
			super(	table,
					new SelectProjection()
					{
						@Override
						public String getProjectionString()
						{
							return "COUNT(*)";
						}
					},
					recordsQuery);
		}
		
	}
	
	/**
	 * A {@link SelectProjection} class for the execution of a the inner query of a {@link ExtremeValueRecordQuery}.
	 * 
	 * @author mstevens
	 */
	private class MinMaxProjection implements SelectProjection
	{
		
		private final SColumn extremeValueSqlCol;
		private final boolean max;
		
		public MinMaxProjection(SColumn extremeValueSqlCol, boolean max)
		{
			this.extremeValueSqlCol = extremeValueSqlCol;
			this.max = max;
		}
		
		@Override
		public String getProjectionString()
		{
			return (max ? "MAX" : "MIN") + "(" + extremeValueSqlCol.name + ")";
		}
		
	}
	
	/**
	 * A {@link RecordValueSetSelectHelper} class for the execution of a {@link ExtremeValueRecordQuery}.
	 * 
	 * @author mstevens
	 */
	protected class ExtremeValueRecordSelectHelper extends RecordValueSetSelectHelper<Record>
	{
		
		private final SColumn extremeValueSqlCol;
		private final SelectHelper<MinMaxProjection> innerQueryHelper;

		/**
		 * @param table
		 * @param extremeValueRecordQuery
		 */
		public ExtremeValueRecordSelectHelper(STable table, ExtremeValueRecordQuery extremeValueRecordQuery)
		{
			// Outer query:
			super(table, new RecordSelectionProjection(table), null, false /*wait with building the query*/);
			
			// Get extremeValueSqlCol:
			this.extremeValueSqlCol = table.getSQLColumn(extremeValueRecordQuery.getColumnPointer());
			if(extremeValueSqlCol == null)
			{
				this.exception = new DBException("Failed to generate SQL for extremeValueRecordQuery on column " + extremeValueRecordQuery.getColumnPointer().getQualifiedColumnName(table.schema));
				innerQueryHelper = null;
				return;
			}
			
			// Inner query:
			innerQueryHelper = new SelectHelper<MinMaxProjection>(table, new MinMaxProjection(extremeValueSqlCol, extremeValueRecordQuery.isMax()), extremeValueRecordQuery.getRecordsQuery(), true);
			
			// Now build the full query:
			buildQuery(new RecordsQuery(table.schema, 1 /*LIMIT 1*/));
		}

		@Override
		protected boolean forceWhereClause()
		{
			return true;
		}

		@Override
		protected void appendWhereClause(Constraint constraints)
		{
			if(exception != null && innerQueryHelper != null)
				return;
			bldr.append("WHERE");
			bldr.append(extremeValueSqlCol.name);
			bldr.append(getComparisonOperator(Comparison.EQUAL));
			bldr.append("(");
			bldr.openTransaction("");
			// Insert subquery:
			try
			{
				bldr.append(innerQueryHelper.getQuery(false));
			}
			catch(DBException dbE)
			{
				this.exception = dbE;
				return;
			}
			// Complete outer query:
			bldr.commitTransaction(false);
			bldr.append(")", false);
		}

		/* (non-Javadoc)
		 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore.StatementHelper#isParameterised()
		 */
		@Override
		protected boolean isParameterised()
		{
			if(innerQueryHelper != null)
				return innerQueryHelper.isParameterised();
			return super.isParameterised();
		}

		/* (non-Javadoc)
		 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore.StatementHelper#getParameterColumns()
		 */
		@Override
		public List<SColumn> getParameterColumns()
		{
			if(innerQueryHelper != null)
				return innerQueryHelper.getParameterColumns();
			return super.getParameterColumns();
		}

		/* (non-Javadoc)
		 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore.RecordsByConstraintsHelper#getSapArguments()
		 */
		@Override
		public List<Object> getSapArguments()
		{
			if(innerQueryHelper != null)
				return innerQueryHelper.getSapArguments();
			return super.getSapArguments();
		}
		
	}

	/**
	 * Helper class to build DELETE statements (parameterised or literal) for multiple records.
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
			appendWhereClause(recordsQuery.getConstraints());
		}
		
	}

}
