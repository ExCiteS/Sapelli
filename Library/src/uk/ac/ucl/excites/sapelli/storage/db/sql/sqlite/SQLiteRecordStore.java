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

import java.io.File;
import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.db.DBException;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.RecordColumn;
import uk.ac.ucl.excites.sapelli.storage.model.Schema;
import uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ByteArrayColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.FloatColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.LineColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.LocationColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.OrientationColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.PolygonColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.TimeStampColumn;

/**
 * @author mstevens
 *
 */
public abstract class SQLiteRecordStore extends SQLRecordStore
{
	
	// Statics----------------------------------------------
	static public final String DATABASE_NAME_SUFFIX = "_Data";
	static public final String BACKUP_SUFFIX = "_Backup";
	static public final String FILE_EXTENSION = "sqlite";

	// Dynamics---------------------------------------------
	private String filename;
	
	public SQLiteRecordStore(StorageClient client, File folder, String baseFilename) throws Exception
	{
		super(client, new SQLiteSchemaInfoFactory());
		this.filename = baseFilename + DATABASE_NAME_SUFFIX;
		File dbFile = new File(folder.getAbsolutePath() + File.separator + filename + '.' + FILE_EXTENSION);
		
		boolean newDB = !dbFile.exists(); 
		if(newDB)
		{
			dbFile.createNewFile();
		}
		initialise(newDB);
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
	
	static class SQLiteSchemaInfoFactory extends TableSpecFactory
	{
		
		@Override
		public void visit(TimeStampColumn dateTimeCol)
		{
			// TODO Auto-generated method stub
			
		}
		
		@Override
		public void visit(ByteArrayColumn byteArrayCol)
		{	
			addColumnSpec(new SQLiteStoredColumn<byte[]>(schema, byteArrayCol, byteArrayCol.getName(), "BLOB")
			{

				@Override
				public String toStorableValueString(byte[] value)
				{
					return null; // TODO
				}
			});
		}
		
		@Override
		public void visit(StringColumn stringCol)
		{
			addColumnSpec(new SQLiteStoredColumn<String>(schema, stringCol, stringCol.getName(), "TEXT")
			{

				@Override
				public String toStorableValueString(String value)
				{
					return '\'' + value.replace("'", "''") + '\''; // TODO charset conversion?
				}
			});
		}
		
		@Override
		public void visit(IntegerColumn intCol)
		{
			addColumnSpec(new SQLiteStoredColumn<Long>(schema, intCol, intCol.getName(), "INTEGER")
			{

				@Override
				public String toStorableValueString(Long value)
				{
					return value.toString();
				}
			});
		}
		
		@Override
		public void visit(FloatColumn floatCol)
		{
			addColumnSpec(new SQLiteStoredColumn<Double>(schema, floatCol, floatCol.getName(), "REAL")
			{

				@Override
				public String toStorableValueString(Double value)
				{
					return value.toString();
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
			addColumnSpec(new SQLiteStoredColumn<Boolean>(schema, boolCol, boolCol.getName(), "BOOLEAN")
			{

				@Override
				public String toStorableValueString(Boolean value)
				{
					return value ? "1" : "0";
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

		@Override
		protected List<String> getTableConstraints(Schema schema)
		{
			// TODO Auto-generated method stub
			return null;
		}
		
	}
	
}
