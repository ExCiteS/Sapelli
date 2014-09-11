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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.shared.db.DBException;
import uk.ac.ucl.excites.sapelli.shared.util.StringUtils;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.db.RecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.RecordColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.LocationColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.OrientationColumn;
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
 * @param <SQLRS>
 * @param <STable>
 */
public abstract class SQLRecordStore<SQLRS extends SQLRecordStore<SQLRS, STable, SColumn>, STable extends SQLRecordStore<SQLRS, STable, SColumn>.SQLTable, SColumn extends SQLColumn<?, ?>> extends RecordStore
{
	
	static private final String SCHEMATA_TABLE_NAME = "Schemata";
	private Map<Schema, STable> sqlTables;

	/**
	 * @param client
	 * @param tableSpecFactory
	 */
	public SQLRecordStore(StorageClient client)
	{
		super(client);
		this.sqlTables = new HashMap<Schema, STable>();
	}
	
	/**
	 * Must be called from subclass constructor!
	 * 
	 * @param newDB whether or not the database file is new (i.e. empty)
	 * @throws Exception
	 */
	protected void initialise(boolean newDB) throws Exception
	{
		if(newDB)
			registerSchema(Schema.META_SCHEMA); // create the table to store schemata
		else
		{
			TableFactory tableFactory = getTableFactory();
			// Initialise schemaInfos:
			for(Record metaSchemaRec : retrieveRecords(Schema.META_SCHEMA))
			{
				Schema schema = Schema.FromMetaRecord(metaSchemaRec);
				sqlTables.put(schema, tableFactory.generateTable(schema));
			}
		}
	}
	
	/**
	 * Checks whether the given {@link Schema} is known, which implies that records of it
	 * can be accepted for storage (i.e. a table has been created to store them).
	 * In this default implementation it is assumed if the DBMS does *not* support concurrent
	 * connections {@link #sqlTables} will always in sync with the actual tables existing
	 * in the database (so no further checks are needed). 
	 * 
	 * @param schema
	 * @return
	 */
	protected boolean isSchemaKnown(Schema schema)
	{
		return	sqlTables.containsKey(schema)
				&& (!supportsConcurrentConnections() || doesTableExist(getTableName(schema)));
	}
	
	protected void registerSchema(Schema schema) throws DBException
	{		
		startTransaction();
		try
		{
			// Insert new record in schemata table, but not for meta schema because in that case the schemata table doesn't exist yet!
			if(schema != Schema.META_SCHEMA)
				doStore(Schema.GetMetaRecord(schema));
			
			// Create table for records of this schema:
			getTable(schema).create();				
		}
		catch(Exception e)
		{
			e.printStackTrace();
			rollbackTransactions();
			// TODO throw e; // re-throw
		}
		commitTransaction();
	}

	protected STable getTable(Schema schema)
	{
		STable sqlTable = sqlTables.get(schema);
		if(sqlTable == null)
		{
			sqlTable = getTableFactory().generateTable(schema);
			sqlTables.put(schema, sqlTable);
		}
		return sqlTable;
	}
	
	protected abstract TableFactory getTableFactory();
	
	protected String getTableName(Schema schema)
	{
		if(schema == Schema.META_SCHEMA)
			return SCHEMATA_TABLE_NAME;
		return "Table_" + schema.getModelID() + '_' + schema.getModelSchemaNumber();
	}
	
	protected Set<Schema> getKnownSchemata()
	{
		return sqlTables.keySet();
	}
	
	protected abstract void executeSQL(String sql) throws DBException;
	
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
	 * Default implementation for SQL databases: first do a SELECT based on the key to verify whether the
	 * record exists, then do either UPDATE or INSERT.
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#doStore(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	protected boolean doStore(Record record) throws DBException
	{
		boolean newRec = retrieveRecord(record.getReference().getRecordQuery()) == null;
		if(newRec)
			getTable(record.getSchema()).insert(record);
		else
			getTable(record.getSchema()).update(record);
		return newRec;
	}

	@Override
	protected void doDelete(Record record) throws DBException
	{
		getTable(record.getSchema()).delete(record);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#retrieveRecords(uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery)
	 */
	@Override
	public List<Record> retrieveRecords(RecordsQuery query)
	{
		List<Record> result = new ArrayList<Record>();
		for(Schema s : query.isAnySchema() ? getKnownSchemata() : query.getSourceSchemata())
			// Run subqueries for each schema in the query, or all known schemata (if the query for "any" schema):
			queryForRecords(getTable(s), query, result);
		return result;
	}
	
	/**
	 * @param schema
	 * @param query
	 * @param result
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

		public final String name;
		public final Schema schema;
		public final Map<ColumnPointer, SColumn> sqlColumns;
		List<String> tableConstraints = Collections.<String> emptyList();
		
		public SQLTable(String tableName, Schema schema)
		{
			this.name = tableName;
			this.schema = schema;
			// Init collections:
			sqlColumns = new LinkedHashMap<ColumnPointer, SColumn>();
		}
		
		public void addColumn(SColumn sqlColumn)
		{
			sqlColumns.put(sqlColumn.sourceColumnPointer, sqlColumn);
		}
		
		public void setTableConstraint(List<String> tableConstraints)
		{
			this.tableConstraints = new ArrayList<String>(tableConstraints);
		}
		
		/**
		 * Default implementation, can be overriden by subclasses
		 * 
		 * @throws DBException
		 */
		public void create() throws DBException
		{
			executeSQL(generateCreateTableStatement());
		}
		
		protected String generateCreateTableStatement()
		{
			StringBuilder bldr = new StringBuilder();
			bldr.append("CREATE TABLE ");
			bldr.append(name);
			bldr.append(" (");
			// Columns:
			boolean first = true;
			for(SQLColumn<?, ?> sqlCol : sqlColumns.values())
			{
				if(first)
					first = false;
				else
					bldr.append(", ");
				bldr.append(sqlCol.name);
				bldr.append(' ');
				bldr.append(sqlCol.type);
				if(sqlCol.constraint != null && !sqlCol.constraint.isEmpty())
				{
					bldr.append(' ');
					bldr.append(sqlCol.constraint);
				}
			}
			// Table constraints:
			for(String tConstr : tableConstraints)
			{
				bldr.append(", ");
				bldr.append(tConstr);
			}		
			bldr.append(");");
			return bldr.toString();
		}
		
		/**
		 * May be overriden
		 * 
		 * @param record
		 * @throws DBException
		 */
		public void insert(Record record) throws DBException
		{
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
			StringBuilder bldr = new StringBuilder();
			bldr.append("INSERT INTO ");
			bldr.append(name);
			bldr.append(" (");
			// Columns:
			boolean first = true;
			for(SQLColumn<?, ?> sqlCol : sqlColumns.values())
			{
				if(first)
					first = false;
				else
					bldr.append(", ");
				bldr.append(sqlCol.name);
			}
			bldr.append(") VALUES (");
			bldr.append(StringUtils.join(values, ", "));
			bldr.append(");");
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
			
		}
		
		/**
		 * May be overriden
		 * 
		 * @param record
		 * @throws DBException
		 */
		public void delete(Record record) throws DBException
		{
			// TODO
		}
		
		public SColumn getSQLColumn(ColumnPointer columnPointer)
		{
			return sqlColumns.get(columnPointer);
		}
		
		public Schema getSchema()
		{
			return schema;
		}
		
		public String toString()
		{
			return "Database table: " + name;
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

		public abstract SQLType toSQLType(SapType value);
		
		public abstract SapType toSapelliType(SQLType value);
		
	}
	
	/**
	 * @author mstevens
	 *
	 */
	public abstract class TableFactory
	{
		
		public STable generateTable(Schema schema)
		{
			return generateTableSpec(schema.getName(), schema);
		}
		
		public abstract STable generateTableSpec(String tableName, Schema schema);
		
	}
	
	/**
	 * This TableFactory implementation uses a column visitor and assumes all
	 * non-composite Sapelli columns will be represented by by exactly 1 SQLColumn.
	 * Composites (i.e. RecordColumns) will be mapped to sets of SQLColumns, each
	 * corresponding to exactly 1 subcolumn. 
	 * 
	 * @author mstevens
	 */
	public abstract class BasicTableFactory extends TableFactory implements ColumnVisitor
	{
		
		protected Schema schema;
		protected STable table;
		
		@Override
		public STable generateTableSpec(String tableName, Schema schema)
		{
			this.schema = schema;
			table = constructTableSpec(tableName, schema);
			schema.accept(this); // traverse schema --> columnSpecs will be added to TableSpec
			table.setTableConstraint(getTableConstraints());
			return table;
		}
		
		/**
		 * @param tableName
		 * @param schema
		 * @param columnSpecs
		 * @param tableConstraints
		 * @return
		 */
		protected abstract STable constructTableSpec(String tableName, Schema schema);
		
		/**
		 * @return
		 */
		protected abstract List<String> getTableConstraints();
		
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
	protected class WhereClauseGenerator implements ConstraintVisitor
	{
		
		STable table;
		StringBuilder bldr;
		String valuePlaceHolder;
		List<String> arguments;
		boolean quoteArgumentsIfNeeded;
		
		/**
		 * Creates a WhereClauseGenerator that will produce a selection clause with literal values.
		 * 
		 * @param table
		 * @param constraint
		 * @param valuePlaceHolder placeholder for parameters, null if literal clause is being generated
		 */
		public WhereClauseGenerator(STable table, Constraint constraint)
		{
			this(table, constraint, null, true);
		}
		
		/**
		 * Creates a WhereClauseGenerator that will produce a selection clause with parameterised values (unless valuePlaceHolder is null)
		 * 
		 * @param table
		 * @param constraint
		 * @param valuePlaceHolder
		 * @param quoteArgumentsIfNeeded
		 */
		public WhereClauseGenerator(STable table, Constraint constraint, String valuePlaceHolder, boolean quoteArgumentsIfNeeded)
		{
			if(constraint == null)
				return;
			this.table = table;
			this.valuePlaceHolder = valuePlaceHolder;
			this.quoteArgumentsIfNeeded = quoteArgumentsIfNeeded;
			if(isParameterised())
				this.arguments = new ArrayList<String>();
			this.bldr = new StringBuilder();
			constraint.accept(this);
		}
		
		protected boolean isParameterised()
		{
			return valuePlaceHolder != null;
		}
		
		private void visitAndOr(boolean and, List<Constraint> subConstraints)
		{
			if(subConstraints.isEmpty())
				return;
			bldr.append("(");
			boolean first = true;
			for(Constraint subConstraint : subConstraints)
			{
				if(first)
					first = false;
				else
					bldr.append(" " + (and ? "AND" : "OR") + " ");
				subConstraint.accept(this);
			}
			bldr.append(")");
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

		@Override
		public void visit(NotConstraint notConstr)
		{
			bldr.append(" NOT ");
			notConstr.getNegatedConstraint().accept(this);
		}

		@Override
		public void visit(EqualityConstraint equalityQuery)
		{
			// TODO
//			ColumnSpec<?> storedCol = schemaInfo.getStoredColumn(equalityQuery.getColumnPointer());
//			bldr.append(storedCol.name);
//			bldr.append(" " + getComparisonOperator(RuleConstraint.Comparison.EQUAL) + " ");
//			bldr.append(storedCol.getStorableValueString(equalityQuery.getValue()));
		}

		@Override
		public void visit(RuleConstraint ruleQuery)
		{
			SQLColumn<?, ?> sqlCol = table.getSQLColumn(ruleQuery.getColumnPointer());
			String literalValue = sqlCol.sapObjectToLiteral(ruleQuery.getValue(), quoteArgumentsIfNeeded);
			bldr.append(sqlCol.name);
			bldr.append(" " + getComparisonOperator(ruleQuery.getComparison()) + " ");
			if(isParameterised())
			{
				bldr.append(valuePlaceHolder);
				arguments.add(literalValue);
			}
			else
				bldr.append(literalValue);
		}
		
		public String getClause(boolean includeWhere)
		{
			if(bldr != null && bldr.length() > 0)
				return (includeWhere ? "WHERE " : "") + bldr.toString();
			else
				return "";
		}
		
		public String getClauseOrNull(boolean includeWhere)
		{
			String clause = getClause(includeWhere);
			return clause.isEmpty() ? null : clause;
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
