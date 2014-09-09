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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import uk.ac.ucl.excites.sapelli.shared.db.DBException;
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
import uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor;

/**
 * @author mstevens
 *
 */
public abstract class SQLRecordStore extends RecordStore
{
	
	static private final String SCHEMATA_TABLE_NAME = "Schemata";
	private Map<Schema, SQLTable> TableSpecs;

	/**
	 * @param client
	 * @param tableSpecFactory
	 */
	public SQLRecordStore(StorageClient client)
	{
		super(client);
		this.TableSpecs = new HashMap<Schema, SQLTable>();
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
				TableSpecs.put(schema, tableFactory.generateTableSpec(schema));
			}
		}
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
		return TableSpecs.keySet();
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
	 * Checks whether the given {@link Schema} is known, which implies that records of it
	 * can be accepted for storage (i.e. a table has been created to store them).
	 * In this default implementation it is assumed if the DBMS does *not* support concurrent
	 * connections {@link #TableSpecs} will always in sync with the actual tables existing
	 * in the database (so no further checks are needed). 
	 * 
	 * @param schema
	 * @return
	 */
	protected boolean isSchemaKnown(Schema schema)
	{
		return	TableSpecs.containsKey(schema)
				&& (!supportsConcurrentConnections() || doesTableExist(getTableName(schema)));
	}
	
	protected void registerSchema(Schema schema) throws DBException
	{		
		startTransaction();
		try
		{
			// Insert new record in schemata table, but not for meta schema because in that case the schemata table doesn't exist yet!
			if(schema != Schema.META_SCHEMA)
				insert(Schema.GetMetaRecord(schema));
			
			// Create table for records of this schema:
			executeSQL(getTable(schema).getCreateTableStatement());				
		}
		catch(Exception e)
		{
			rollbackTransactions();
			// TODO throw e; // re-throw
		}
		commitTransaction();
	}
	
	@Override
	protected boolean doStore(Record record) throws DBException
	{
		
		// Register schema if we don't know it yet:
		if(!isSchemaKnown(record.getSchema()))
			registerSchema(record.getSchema());
		
		boolean newRec = true; // TODO insert or update?
		if(newRec)
			insert(record);
		else
			update(record);
		return newRec;
	}

	protected void update(Record record) throws DBException
	{
		// TODO
	}
	
	protected void insert(Record record) throws DBException
	{
		SQLTable table = getTable(record.getSchema());
		SQLStatement insert = table.getInsertStatement();
		// TODO bind record column values to statement params
		// TODO execute statement
	}

	private SQLTable getTable(Schema schema)
	{
		SQLTable schemaInfo = TableSpecs.get(schema);
		if(schemaInfo == null)
		{
			schemaInfo = getTableFactory().generateTableSpec(schema);
			TableSpecs.put(schema, schemaInfo);
		}
		return schemaInfo;
	}
	
	@Override
	protected void doDelete(Record record) throws DBException
	{
		// TODO Auto-generated method stub
		
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#retrieveRecords(uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery)
	 */
	@Override
	public List<Record> retrieveRecords(RecordsQuery query)
	{
		List<Record> result = new ArrayList<Record>();
		for(Schema s : query.isAnySchema() ? getKnownSchemata() : query.getSourceSchemata())
			// run subqueries for each schema in the query, or all known schemata (if the query for "any" schema):
			queryForRecords(s, query, result);
		return result;
	}
	
	private void queryForRecords(Schema schema, RecordsQuery query, List<Record> result)
	{
		String selectStatement = "SELECT * FROM " + getTableName(schema) + (new WhereClauseGenerator(getTable(schema), query.getConstraints())).getClause(); 
		// TODO + (query.getOrderBy() != null ? 
		// TODO limit
		// TODO generate & execute SELECT query + process & add to result list
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#retrieveRecord(uk.ac.ucl.excites.sapelli.storage.queries.SingleRecordQuery)
	 */
	@Override
	public Record retrieveRecord(SingleRecordQuery query)
	{
		// TODO generate & execute SELECT query + process & return result
				
		return null;
	}
	
	private class WhereClauseGenerator implements ConstraintVisitor
	{

		SQLTable schemaInfo;
		StringBuilder bldr;
		
		public WhereClauseGenerator(SQLTable schemaInfo, Constraint constraint)
		{
			this.schemaInfo = schemaInfo;
			if(constraint != null)
			{
				this.bldr = new StringBuilder();
				constraint.accept(this);
			}
		}
		
		private void visitAndOr(boolean and, List<Constraint> subConstraints)
		{
			bldr.append(" (");
			boolean first = true;
			for(Constraint subConstraint : subConstraints)
			{
				if(first)
					first = false;
				else
					bldr.append("" + (and ? "AND" : "OR"));
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
			// TODO
//			ColumnSpec<?> storedCol = schemaInfo.getStoredColumn(ruleQuery.getColumnPointer());
//			bldr.append(storedCol.name);
//			bldr.append(" " + getComparisonOperator(ruleQuery.getComparison()) + " ");
//			bldr.append(storedCol.getStorableValueString(ruleQuery.getValue()));
		}
		
		public String getClause()
		{
			if(bldr != null && bldr.length() > 0)
				return "WHERE " + bldr.toString();
			else
				return "";
		}
		
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
	 */
	public abstract class TableFactory implements ColumnVisitor
	{
		
		protected Schema schema;
		protected SQLTable tableSpec;
		
		public SQLTable generateTableSpec(Schema schema)
		{
			return generateTableSpec(schema.getName(), schema);
		}
		
		public SQLTable generateTableSpec(String tableName, Schema schema)
		{
			this.schema = schema;
			tableSpec = constructTableSpec(tableName, schema);
			schema.accept(this); // traverse schema --> columnSpecs will be added to TableSpec
			tableSpec.setTableConstraint(getTableConstraints(schema));
			return tableSpec;
		}
		
		/**
		 * @param tableName
		 * @param schema
		 * @param columnSpecs
		 * @param tableConstraints
		 * @return
		 */
		protected abstract SQLTable constructTableSpec(String tableName, Schema schema);
		
		/**
		 * @return
		 */
		protected abstract List<String> getTableConstraints(Schema schema);
		
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

}
