/**
 * 
 */
package uk.ac.ucl.excites.sapelli.storage.db;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import uk.ac.ucl.excites.sapelli.storage.StorageClient;
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
	
	private final Map<Schema,SchemaInfo> schemaInfos;

	/**
	 * @param client
	 */
	public SQLRecordStore(StorageClient client)
	{
		super(client);
		this.schemaInfos = new HashMap<Schema, SQLRecordStore.SchemaInfo>();
	}
	
	private SchemaInfo getSchemaInfo(Schema schema)
	{
		SchemaInfo schemaInfo = schemaInfos.get(schema);
		if(schemaInfo == null)
		{
			schemaInfo = new SchemaInfo(schema, getSchemaInfoGenerator());
			schemaInfos.put(schema, schemaInfo);
		}
		return schemaInfo;
	}
	
	protected abstract SchemaInfoGenerator getSchemaInfoGenerator();
	
	protected abstract ValueStringGenerator getValueStringGenerator();
	
	protected String getTableName(Schema schema)
	{
		if(schema == Schema.META_SCHEMA)
			return "Schemata";
		return "Table_" + schema.getModelID() + '_' + schema.getModelSchemaNumber();
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
	
	protected abstract boolean doesTableExist(String tableName);
	
	protected abstract void executeQuery(String sql) throws Exception;
	
	protected void registerSchema(Schema schema) throws Exception
	{		
		if(!doesTableExist(getTableName(schema)))
		{				
			//TODO start transaction
			
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
				// TODO rollback transaction
				
				throw e; // re-throw
			}
			
			// TODO commit transaction
		}
	}
	
	protected List<Schema> getKnownSchemata()
	{
		// SELECT * FROM getTableName(Schema.META_SCHEMA);
		return null;
	}
	
	protected void insert(Record record) throws Exception
	{
		executeQuery(getSchemaInfo(record.getSchema()).getInsertStatement(record));
	}
	
	protected void insertOrUpdate(Record record)
	{
		// TODO
	}
	
	protected void uponDatabaseCreation() throws Exception
	{
		registerSchema(Schema.META_SCHEMA); // create the table to store schemata
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#store(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	public void store(Record record)
	{
		insertOrUpdate(record);
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
		String selectStatement = "SELECT * FROM " + getTableName(schema) + (new WhereClauseGenerator(getSchemaInfo(schema), query.getConstraints(), getValueStringGenerator())).getClause(); // + (query.getOrderBy() != null ? 
		
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

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#deleteAllRecords()
	 */
	@Override
	public void deleteAllRecords()
	{
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#delete(uk.ac.ucl.excites.sapelli.storage.model.Record)
	 */
	@Override
	public void delete(Record record)
	{
		// TODO Auto-generated method stub

	}
	
	protected class SchemaInfo
	{
		
		Schema schema;
		List<ColumnPointer> storedColumns; // in order
		Map<ColumnPointer,String> columnNames;
		Map<ColumnPointer,String> columnTypes;
		
		public SchemaInfo(Schema schema, SchemaInfoGenerator generator)
		{
			this.schema = schema;
			this.storedColumns = new ArrayList<ColumnPointer>();
			this.columnNames = new HashMap<ColumnPointer, String>();
			this.columnTypes = new HashMap<ColumnPointer, String>();
			generator.initialise(this);
		}
		
		public void addColumn(ColumnPointer columnPointer, String name, String type)
		{
			storedColumns.add(columnPointer);
			columnNames.put(columnPointer, name);
			columnTypes.put(columnPointer, type);
		}
		
		public String getCreateTableStatement()
		{
			// name, type, nullable, indexed ...
			
			return null;
		}
		
		public String getInsertStatement(Record record)
		{			
			StringBuilder bldr = new StringBuilder();
			bldr.append("INSERT INTO ");
			bldr.append(getTableName(record.getSchema()));
			bldr.append(" (");
			for(ColumnPointer cp : storedColumns)
			{
				// not needed if we always write a value for each col (in order!)
			}
			bldr.append(") VALUES (");
			for(ColumnPointer cp : storedColumns)
			{
				
			}
			bldr.append(");");
			return bldr.toString();
		}
		
		public String getColumnName(ColumnPointer columnPointer)
		{
			return columnNames.get(columnPointer);
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
	
	public abstract class ValueStringGenerator implements ColumnVisitor
	{
		
		protected Object value;
		protected String valueString;
		
		public String getValueString(ColumnPointer columnPointer, Object value)
		{
			this.value = value;
			this.valueString = null;
			
			// Get string for column type:
			columnPointer.getColumn().accept(this);
			
			return this.valueString;
		}
		
	}
	
	private class WhereClauseGenerator implements ConstraintVisitor
	{

		SchemaInfo schemaInfo;
		ValueStringGenerator valueStringGenerator;
		StringBuilder bldr;
		
		public WhereClauseGenerator(SchemaInfo schemaInfo, Constraint constraint, ValueStringGenerator valueStringGenerator)
		{
			this.schemaInfo = schemaInfo;
			if(constraint != null)
			{
				this.bldr = new StringBuilder();
				this.valueStringGenerator = valueStringGenerator;
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
			bldr.append(schemaInfo.getColumnName(equalityQuery.getColumnPointer()));
			bldr.append(" " + getComparisonOperator(RuleConstraint.Comparison.EQUAL) + " ");
			bldr.append(valueStringGenerator.getValueString(equalityQuery.getColumnPointer(), equalityQuery.getValue())); //TODO what if this cp does not exist as such in the table? Consult SchemaInfo for mapping?
		}

		@Override
		public void visit(RuleConstraint ruleQuery)
		{
			bldr.append(schemaInfo.getColumnName(ruleQuery.getColumnPointer()));
			bldr.append(" " + getComparisonOperator(ruleQuery.getComparison()) + " ");
			bldr.append(valueStringGenerator.getValueString(ruleQuery.getColumnPointer(), ruleQuery.getValue())); //TODO what if this cp does not exist as such in the table? Consult SchemaInfo for mapping?
		}
		
		public String getClause()
		{
			if(bldr != null && bldr.length() > 0)
				return "WHERE " + bldr.toString();
			else
				return "";
		}
		
	}

}
