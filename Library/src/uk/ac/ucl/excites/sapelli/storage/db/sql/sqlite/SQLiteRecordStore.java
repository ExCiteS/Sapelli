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

package uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite;

import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.db.DBException;
import uk.ac.ucl.excites.sapelli.shared.util.TransactionalStringBuilder;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore.TypeMapping;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.types.SQLiteBlobColumn;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.types.SQLiteBooleanColumn;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.types.SQLiteDoubleColumn;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.types.SQLiteIntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.types.SQLiteStringColumn;
import uk.ac.ucl.excites.sapelli.storage.model.AutoIncrementingPrimaryKey;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Index;
import uk.ac.ucl.excites.sapelli.storage.model.PrimaryKey;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ByteArrayColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.FloatColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.LineColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.PolygonColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.TimeStampColumn;
import uk.ac.ucl.excites.sapelli.storage.types.TimeStamp;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;

/**
 * @author mstevens
 *
 */
public abstract class SQLiteRecordStore extends SQLRecordStore<SQLiteRecordStore, SQLiteRecordStore.SQLiteTable, SQLiteRecordStore.SQLiteColumn<?, ?>>
{
	
	// Dynamics---------------------------------------------
	private final SQLiteTableFactory factory;
	
	public SQLiteRecordStore(StorageClient client) throws Exception
	{
		super(client);
		factory = new SQLiteTableFactory();
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore#getTableFactory()
	 */
	@Override
	protected TableFactory getTableFactory()
	{
		return factory;
	}

	/**
	 * In SQlite basic transactions (those controlled with BEGIN...COMMIT/ROLLBACK) cannot
	 * be nested (for that one needs to use the SAVEPOINT and RELEASE commands, which we won't
	 * use here). However, for flexibility reasons we will pretend that it is possible (i.e. we
	 * don't throw an exception if a request arrives to open a 2nd, 3rd, etc. transaction).
	 * 
	 * @see <a href="http://sqlite.org/lang_transaction.html">http://sqlite.org/lang_transaction.html</a>
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.db.RecordStore#doStartTransaction()
	 */
	@Override
	protected void doStartTransaction() throws DBException
	{
		if(!isInTransaction())
			try
			{
				executeSQL("BEGIN TRANSACTION;");
			}
			catch(Exception ex)
			{
				throw new DBException("Could not open SQLite transaction", ex);
			}
	}

	@Override
	protected void doCommitTransaction() throws DBException
	{
		if(getOpenTransactions() == 1) // higher numbers indicate nested transactions which are simulated
			try
			{
				executeSQL("COMMIT TRANSACTION;");
			}
			catch(Exception ex)
			{
				throw new DBException("Could not commit SQLite transaction", ex);
			}
	}

	@Override
	protected void doRollbackTransaction() throws DBException
	{
		if(getOpenTransactions() == 1) // higher numbers indicate nested transactions which are simulated
			try
			{
				executeSQL("ROLLBACK TRANSACTION;");
			}
			catch(Exception ex)
			{
				throw new DBException("Could not roll-back SQLite transaction", ex);
			}
	}

	@Override
	protected boolean doesTableExist(String tableName)
	{
		// TODO Auto-generated method stub
		return false;
		// SELECT name FROM sqlite_master WHERE type='table' AND name='table_name';
	}
	
	/**
	 * @param sql
	 * @return
	 */
	protected abstract ISQLiteStatement newSQLiteStatement(String sql);
	
	/**
	 * @return String used to mark unbound parameters in parameterised statements/queries
	 */
	protected abstract String getParameterPlaceHolder();
	
	/**
	 * @author mstevens
	 *
	 */
	public class SQLiteTable extends SQLRecordStore<SQLiteRecordStore, SQLiteRecordStore.SQLiteTable, SQLiteRecordStore.SQLiteColumn<?, ?>>.SQLTable
	{

		private ISQLiteStatement insertStatement;

		public SQLiteTable(Schema schema)
		{
			super(schema);
		}

		/* (non-Javadoc)
		 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore.SQLTable#insert(uk.ac.ucl.excites.sapelli.storage.model.Record)
		 */
		@Override
		public void insert(Record record) throws DBException
		{
			if(insertStatement == null)
				insertStatement = newSQLiteStatement(generateInsertStatement(getParameterPlaceHolder()));
			else
				insertStatement.clearAllBindings(); // clear bindings for reuse

			// Bind parameters:
			int i = 0;
			for(SQLiteColumn<?, ?> sqliteCol : sqlColumns.values())
				sqliteCol.retrieveAndBind(insertStatement, i++, record);
			
			// Execute:
			insertStatement.execute();
		}
		
		public void upsert(Record record) throws DBException
		{
			// TODO first read http://stackoverflow.com/questions/3634984/insert-if-not-exists-else-update 
			// and http://stackoverflow.com/questions/418898/sqlite-upsert-not-insert-or-replace
		}

		/* (non-Javadoc)
		 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore.SQLTable#update(uk.ac.ucl.excites.sapelli.storage.model.Record)
		 */
		@Override
		public void update(Record record) throws DBException
		{
			// TODO Auto-generated method stub
			super.update(record);
		}

		/* (non-Javadoc)
		 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore.SQLTable#delete(uk.ac.ucl.excites.sapelli.storage.model.Record)
		 */
		@Override
		public void delete(Record record) throws DBException
		{
			// TODO Auto-generated method stub
			super.delete(record);
		}

	}
	
	/**
	 * @author mstevens
	 *
	 * @param <SQLType>
	 * @param <SapType>
	 */
	public abstract class SQLiteColumn<SQLType, SapType> extends SQLRecordStore<SQLiteRecordStore, SQLiteRecordStore.SQLiteTable, SQLiteRecordStore.SQLiteColumn<SQLType, SapType>>.SQLColumn<SQLType, SapType>
	{

		/**
		 * @param type
		 * @param constraint
		 * @param sourceSchema
		 * @param sourceColumn
		 * @param mapping - may be null in case SQLType = SapType
		 */
		public SQLiteColumn(String type, String constraint, Schema sourceSchema, Column<SapType> sourceColumn, TypeMapping<SQLType, SapType> mapping)
		{
			super(type, constraint, sourceSchema, sourceColumn, mapping);
		}
		
		/**
		 * @param name
		 * @param type
		 * @param constraint
		 * @param sourceColumnPointer
		 * @param mapping - may be null in case SQLType = SapType
		 */
		public SQLiteColumn(String name, String type, String constraint, ColumnPointer sourceColumnPointer, TypeMapping<SQLType, SapType> mapping)
		{
			super(name, type, constraint, sourceColumnPointer, mapping);
		}

		/* (non-Javadoc)
		 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.SQLColumn#sanitiseName(java.lang.String)
		 */
		@Override
		protected String sanitiseIdentifier(String name)
		{
			// RegEx: [a-zA-Z_]+[0-9a-zA-Z_]*
			
			// TODO Auto-generated method stub
			return null;
		}

		/**
		 * @param statement
		 * @param paramIdx
		 * @param record
		 */
		public void retrieveAndBind(ISQLiteStatement statement, int paramIdx, Record record)
		{
			bind(statement, paramIdx, retrieve(record));
		}
		
		/**
		 * @param paramIdx
		 * @param column
		 * @param value
		 */
		public void bind(ISQLiteStatement statement, int paramIdx, SQLType value)
		{
			if(value != null)
				bindNonNull(statement, paramIdx, value);
			else
				statement.bindNull(paramIdx);
		}
		
		protected abstract void bindNonNull(ISQLiteStatement statement, int paramIdx, SQLType value);

		@Override
		protected String getNullString()
		{
			return "null";
		}

		@Override
		protected String getQuoteChar()
		{
			return "'";
		}

		@Override
		protected String getQuoteEscape()
		{
			return "''";
		}
		
		/**
		 * @param record
		 * @param cursor
		 * @param columnIdx
		 */
		public void storeFrom(Record record, ISQLiteCursor cursor, int columnIdx)
		{
			store(record, readFrom(cursor, columnIdx));
		}
		
		public SQLType readFrom(ISQLiteCursor cursor, int columnIdx)
		{
			if(cursor.isNull(columnIdx))
				return null;
			return getFrom(cursor, columnIdx);
		}
		
		protected abstract SQLType getFrom(ISQLiteCursor cursor, int columnIdx);

	}
	
	/**
	 * @author mstevens
	 *
	 */
	protected class SQLiteTableFactory extends BasicTableFactory
	{
		
		private List<Index> singleColumnIndexes = new ArrayList<Index>();
		
		@Override
		protected SQLiteTable newTableSpec(Schema schema)
		{
			return new SQLiteTable(schema);
		}
		
		private <SapT> String getColumnConstraint(Column<SapT> sourceColum)
		{
			TransactionalStringBuilder bldr = new TransactionalStringBuilder(SPACE);
			
			// Primary key:
			PrimaryKey pk = schema.getPrimaryKey();
			if(pk != null /* just in case*/ && !pk.isMultiColumn() && pk.containsColumn(sourceColum, false))
			{
				bldr.append("PRIMARY KEY");
				// ASC/DESC?
				// conflict-clause?
				if(pk instanceof AutoIncrementingPrimaryKey)
					bldr.append("AUTOINCREMENT");
			}
			
			// Regular single-column, unique index (unnamed):
			Index idx = schema.getIndex(sourceColum);
			if(idx != null && !idx.isMultiColumn() && idx.isUnique())
			{
				bldr.append("UNIQUE");
				// conflict-clause?
				// Remember that this index has been handled:
				singleColumnIndexes.add(idx);
			}
				
			// Optionality:
			if(!sourceColum.isOptional())
				bldr.append("NOT NULL");
			
			// TODO Default value?
			
			// TODO foreign-key-clause?
			if(sourceColum instanceof ForeignKeyColumn)
			{
				// ...
			}
			
			return bldr.toString();
		}
		
		protected List<String> getTableConstraints()
		{
			// TODO indexes, etc.
			
			List<String> tConstraints = new ArrayList<String>();
			for(Index idx : schema.getIndexes())
				if(!singleColumnIndexes.contains(idx))
				{
					// TODO
				}
			
			return tConstraints;
		}
		
		/* (non-Javadoc)
		 * @see uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn)
		 * @see Section 1.1 in http://www.sqlite.org/datatype3.html
		 */
		@Override
		public void visit(BooleanColumn boolCol)
		{
			//table.addColumn(new SQLiteBooleanColumn(getColumnConstraint(boolCol), schema, boolCol));
		}
		
		@Override
		public void visit(final TimeStampColumn timeStampCol)
		{
//			table.addColumn(new SQLiteStringColumn<TimeStamp>(getColumnConstraint(timeStampCol), schema, timeStampCol, new TypeMapping<String, TimeStamp>()
//			{
//
//				@Override
//				public String toSQLType(TimeStamp value)
//				{
//					return timeStampCol.toString(value);
//				}
//
//				@Override
//				public TimeStamp toSapelliType(String value)
//				{
//					return timeStampCol.parse(value);
//				}
//				
//			}));
		}
		
		@Override
		public void visit(ByteArrayColumn byteArrayCol)
		{
			//table.addColumn(new SQLiteBlobColumn<byte[]>(getColumnConstraint(byteArrayCol), schema, byteArrayCol, null));
		}
		
		@Override
		public void visit(StringColumn stringCol)
		{
			//table.addColumn(new SQLiteStringColumn<String>(getColumnConstraint(stringCol), schema, stringCol, null));
		}
		
		@Override
		public void visit(IntegerColumn intCol)
		{
			//table.addColumn(new SQLiteIntegerColumn<Long>(getColumnConstraint(intCol), schema, intCol, null));
		}
		
		@Override
		public void visit(FloatColumn floatCol)
		{
			//table.addColumn(new SQLiteDoubleColumn<Double>(getColumnConstraint(floatCol), schema, floatCol, null));
		}
		
		@Override
		public void visit(PolygonColumn polyCol)
		{
			// TODO Auto-generated method stub
			
		}
		
		@Override
		public void visit(LineColumn lineCol)
		{
			// TODO Auto-generated method stub
			
		}
		
		@Override
		public void visit(IntegerListColumn intListCol)
		{
			// TODO Auto-generated method stub
			
		}
		
	}
	
}
