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

package uk.ac.ucl.excites.sapelli.collector.db;

import java.io.File;

import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.RecordColumn;
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
 * @param <T>
 *
 */
public class SQLiteRecordStore extends SQLRecordStore
{

	public SQLiteRecordStore(StorageClient client) throws Exception
	{
		super(client);
		// TODO Auto-generated constructor stub
		
		uponDatabaseCreation(); // TODO only when new file
	}

	@Override
	public void startTransaction()
	{
		try
		{
			executeQuery("BEGIN TRANSACTION");
		}
		catch(Exception ex)
		{
			ex.printStackTrace(System.err);
		}
	}

	@Override
	public void commitTransaction()
	{
		try
		{
			executeQuery("COMMIT TRANSACTION");
		}
		catch(Exception ex)
		{
			ex.printStackTrace(System.err);
		}
	}

	@Override
	public void rollbackTransaction()
	{
		try
		{
			executeQuery("ROLLBACK TRANSACTION");
		}
		catch(Exception ex)
		{
			ex.printStackTrace(System.err);
		}
	}
	
	@Override
	public void finalise()
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public void backup(File destinationFolder) throws Exception
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	protected boolean doesTableExist(String tableName)
	{
		// TODO Auto-generated method stub
		return false;
		// SELECT name FROM sqlite_master WHERE type='table' AND name='table_name';
	}

	@Override
	protected void executeQuery(String sql) throws Exception
	{
		System.out.println("Execute SQL: " + sql);
	}
	
	@Override
	protected SchemaInfoGenerator getSchemaInfoGenerator()
	{
		return new SchemaInfoGenerator()
		{
			
			@Override
			public void visit(TimeStampColumn dateTimeCol)
			{
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visit(ByteArrayColumn byteArrayCol)
			{	
				schemaInfo.addStoredColumn(new SQLiteStoredColumn<byte[]>(schemaInfo.getSchema(), byteArrayCol, byteArrayCol.getName(), "BLOB")
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
				schemaInfo.addStoredColumn(new SQLiteStoredColumn<String>(schemaInfo.getSchema(), stringCol, stringCol.getName(), "TEXT")
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
				schemaInfo.addStoredColumn(new SQLiteStoredColumn<Long>(schemaInfo.getSchema(), intCol, intCol.getName(), "INTEGER")
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
				schemaInfo.addStoredColumn(new SQLiteStoredColumn<Double>(schemaInfo.getSchema(), floatCol, floatCol.getName(), "REAL")
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
				schemaInfo.addStoredColumn(new SQLiteStoredColumn<Boolean>(schemaInfo.getSchema(), boolCol, boolCol.getName(), "BOOLEAN")
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
			
		};
	}

}
