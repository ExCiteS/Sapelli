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
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
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
 */
public abstract class SQLRecordStore extends RecordStore
{
	
	static private final String SCHEMATA_TABLE_NAME = "Schemata";
	
	private Map<Schema,SchemaInfo> schemaInfos;

	/**
	 * @param client
	 */
	public SQLRecordStore(StorageClient client)
	{
		super(client);
		this.schemaInfos = new HashMap<Schema, SQLRecordStore.SchemaInfo>();
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
			// Initialise schemaInfos:
			for(Record metaSchemaRec : retrieveRecords(Schema.META_SCHEMA))
			{
				Schema schema = Schema.FromMetaRecord(metaSchemaRec);
				schemaInfos.put(schema, new SchemaInfo(schema));
			}
		}
	}
	
	protected String getTableName(Schema schema)
	{
		if(schema == Schema.META_SCHEMA)
			return SCHEMATA_TABLE_NAME;
		return "Table_" + schema.getModelID() + '_' + schema.getModelSchemaNumber();
	}
	
	protected Set<Schema> getKnownSchemata()
	{
		return schemaInfos.keySet();
	}
	
	protected abstract void executeQuery(String sql) throws DBException;
	
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
	 * connections {@link #schemaInfos} will always in sync with the actual tables existing
	 * in the database (so no further checks are needed). 
	 * 
	 * @param schema
	 * @return
	 */
	protected boolean isSchemaKnown(Schema schema)
	{
		return	schemaInfos.containsKey(schema)
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
			executeQuery(getSchemaInfo(schema).getCreateTableStatement());				
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
		// Insert record:
		executeQuery(getSchemaInfo(record.getSchema()).getInsertStatement(record));
	}

	private SchemaInfo getSchemaInfo(Schema schema)
	{
		SchemaInfo schemaInfo = schemaInfos.get(schema);
		if(schemaInfo == null)
		{
			schemaInfo = new SchemaInfo(schema);
			schemaInfos.put(schema, schemaInfo);
		}
		return schemaInfo;
	}
	
	protected abstract SchemaInfoGenerator getSchemaInfoGenerator();
	
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
		String selectStatement = "SELECT * FROM " + getTableName(schema) + (new WhereClauseGenerator(getSchemaInfo(schema), query.getConstraints())).getClause(); // + (query.getOrderBy() != null ? 
		
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
	
	protected class SchemaInfo
	{
		
		private final Schema schema;
		private final List<StoredColumn<?>> storedColumns; // in order
		private final Map<ColumnPointer, StoredColumn<?>> columnMapping;
		
		public SchemaInfo(Schema schema)
		{
			this.schema = schema;
			this.storedColumns = new ArrayList<StoredColumn<?>>();
			this.columnMapping = new HashMap<ColumnPointer, StoredColumn<?>>();
			getSchemaInfoGenerator().initialise(this);
		}
		
		public void addStoredColumn(StoredColumn<?> storedCol)
		{
			storedColumns.add(storedCol);
			columnMapping.put(storedCol.sourceColumnPointer, storedCol);
		}
		
		public String getCreateTableStatement()
		{
			StringBuilder bldr = new StringBuilder();
			bldr.append("CREATE TABLE ");
			bldr.append(getTableName(schema));
			bldr.append(" (");
			for(StoredColumn<?> c : storedColumns)
			{
				if(c != storedColumns.get(0))
					bldr.append(", ");
				bldr.append(c.name);
				bldr.append(' ');
				bldr.append(c.spec);
			}
			// TODO primary key, ...
			bldr.append(");");
			return bldr.toString();
		}
		
		public String getInsertStatement(Record record)
		{			
			StringBuilder bldr = new StringBuilder();
			bldr.append("INSERT INTO ");
			bldr.append(getTableName(record.getSchema()));
			bldr.append(" (");
			for(StoredColumn<?> c : storedColumns)
			{
				if(c != storedColumns.get(0))
					bldr.append(", ");
				bldr.append(c.name);
			}
			bldr.append(") VALUES (");
			for(StoredColumn<?> c : storedColumns)
			{
				if(c != storedColumns.get(0))
					bldr.append(", ");
				bldr.append(c.getStorableValueString(record));
			}
			bldr.append(");");
			return bldr.toString();
		}
		
		public StoredColumn<?> getStoredColumn(ColumnPointer columnPointer)
		{
			return columnMapping.get(columnPointer);
		}
		
		public Schema getSchema()
		{
			return schema;
		}
		
	}
	
	public abstract class SchemaInfoGenerator implements ColumnVisitor
	{
		
		protected SchemaInfo schemaInfo;
		
		public void initialise(SchemaInfo schemaInfo)
		{
			this.schemaInfo = schemaInfo;
			schemaInfo.schema.accept(this); // traverse schema
		}
		
	}
	
	private class WhereClauseGenerator implements ConstraintVisitor
	{

		SchemaInfo schemaInfo;
		StringBuilder bldr;
		
		public WhereClauseGenerator(SchemaInfo schemaInfo, Constraint constraint)
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
			StoredColumn<?> storedCol = schemaInfo.getStoredColumn(equalityQuery.getColumnPointer());
			bldr.append(storedCol.name);
			bldr.append(" " + getComparisonOperator(RuleConstraint.Comparison.EQUAL) + " ");
			bldr.append(storedCol.getStorableValueString(equalityQuery.getValue()));
		}

		@Override
		public void visit(RuleConstraint ruleQuery)
		{
			StoredColumn<?> storedCol = schemaInfo.getStoredColumn(ruleQuery.getColumnPointer());
			bldr.append(storedCol.name);
			bldr.append(" " + getComparisonOperator(ruleQuery.getComparison()) + " ");
			bldr.append(storedCol.getStorableValueString(ruleQuery.getValue()));
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

}
