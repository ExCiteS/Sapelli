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

import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.db.DBException;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.db.sql.ColumnMapping;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLTable;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.types.SQLiteBlobColumn;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.types.SQLiteDoubleColumn;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.types.SQLiteIntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.types.SQLiteStringColumn;
import uk.ac.ucl.excites.sapelli.storage.model.AutoIncrementingPrimaryKey;
import uk.ac.ucl.excites.sapelli.storage.model.Column;
import uk.ac.ucl.excites.sapelli.storage.model.Index;
import uk.ac.ucl.excites.sapelli.storage.model.PrimaryKey;
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

/**
 * @author mstevens
 *
 */
public abstract class SQLiteRecordStore extends SQLRecordStore
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
	protected abstract SQLiteStatement createStatement(String sql);
	
	protected abstract char getParamPlaceholder();
	
	static private <T> String GetColumnConstraint(Schema schema, Column<T> sourceColum)
	{
		StringBuilder bldr = new StringBuilder();
		
		// Primary key:
		PrimaryKey pk = schema.getPrimaryKey();
		if(pk != null /* just in case*/ && !pk.isMultiColumn() && pk.containsColumn(sourceColum, false))
		{
			bldr.append(" PRIMARY KEY");
			// ASC/DESC?
			// conflict-clause?
			if(pk instanceof AutoIncrementingPrimaryKey)
				bldr.append(" AUTOINCREMENT");
		}
		
		// Regular single-column, unique index (unnamed):
		Index idx = schema.getIndex(sourceColum);
		if(idx != null && !idx.isMultiColumn() && idx.isUnique())
		{
			bldr.append(" UNIQUE");
			// conflict-clause?
		}
			
		// Optionality:
		if(sourceColum.isOptional())
			bldr.append(" NOT NULL");
		
		// TODO Default value?
		
		// TODO foreign-key-clause?
		if(sourceColum instanceof ForeignKeyColumn)
		{
			// ...
		}
		
		return bldr.toString();
	}
	
	/**
	 * @author mstevens
	 *
	 */
	protected class SQLiteTableFactory extends TableFactory
	{
		
		@Override
		protected SQLTable constructTableSpec(String tableName, Schema schema)
		{
			return new SQLiteTable(tableName, schema, SQLiteRecordStore.this);
		}
		
		protected List<String> getTableConstraints(Schema schema)
		{
			return null;
		}
		
		@Override
		public void visit(TimeStampColumn dateTimeCol)
		{
			// TODO Auto-generated method stub
			
		}
		
		@Override
		public void visit(ByteArrayColumn byteArrayCol)
		{	
			tableSpec.addColumnMapping(new ColumnMapping<byte[], byte[]>(schema, byteArrayCol, new SQLiteBlobColumn(byteArrayCol.getName(), GetColumnConstraint(schema, byteArrayCol)))
			{

				@Override
				protected byte[] toDatabaseType(byte[] value)
				{
					return value;
				}

				@Override
				protected byte[] toSapelliType(byte[] value)
				{
					return value;
				}
			});
		}
		
		@Override
		public void visit(StringColumn stringCol)
		{
			tableSpec.addColumnMapping(new ColumnMapping<String, String>(schema, stringCol, new SQLiteStringColumn(stringCol.getName(), GetColumnConstraint(schema, stringCol)))
			{

				@Override
				protected String toDatabaseType(String value)
				{
					return value;
				}

				@Override
				protected String toSapelliType(String value)
				{
					return value;
				}

			});
		}
		
		@Override
		public void visit(IntegerColumn intCol)
		{
			tableSpec.addColumnMapping(new ColumnMapping<Long, Long>(schema, intCol, new SQLiteIntegerColumn(intCol.getName(), GetColumnConstraint(schema, intCol)))
			{

				@Override
				protected Long toDatabaseType(Long value)
				{
					return value;
				}

				@Override
				protected Long toSapelliType(Long value)
				{
					return value;
				}

			});
		}
		
		@Override
		public void visit(FloatColumn floatCol)
		{
			tableSpec.addColumnMapping(new ColumnMapping<Double, Double>(schema, floatCol, new SQLiteDoubleColumn(floatCol.getName(), GetColumnConstraint(schema, floatCol)))
			{

				@Override
				protected Double toDatabaseType(Double value)
				{
					return value;
				}

				@Override
				protected Double toSapelliType(Double value)
				{
					return value;
				}
			});
		}
		
		/* (non-Javadoc)
		 * @see uk.ac.ucl.excites.sapelli.storage.visitors.ColumnVisitor#visit(uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn)
		 * @see Section 1.1 in http://www.sqlite.org/datatype3.html
		 */
		@Override
		public void visit(BooleanColumn boolCol)
		{	
			tableSpec.addColumnMapping(new ColumnMapping<Boolean, Long>(schema, boolCol, SQLiteIntegerColumn.newBooleanColumn(boolCol.getName(), GetColumnConstraint(schema, boolCol)))
			{

				@Override
				protected Long toDatabaseType(Boolean value)
				{
					return (long) (value ? 1 : 0);
				}

				@Override
				protected Boolean toSapelliType(Long value)
				{
					return value == 1;
				}

			});
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
