/**
 * 
 */
package uk.ac.ucl.excites.sapelli.collector.db;

import java.io.File;

import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.db.SQLRecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.RecordColumn;
import uk.ac.ucl.excites.sapelli.storage.model.VirtualColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.BooleanColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ByteArrayColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.DateTimeColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.FloatColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.ForeignKeyColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.IntegerListColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.LineColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.LocationColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.OrientationColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.PolygonColumn;
import uk.ac.ucl.excites.sapelli.storage.model.columns.StringColumn;
import uk.ac.ucl.excites.sapelli.storage.util.ColumnPointer;

/**
 * @author mstevens
 *
 */
public class SQLiteRecordStore extends SQLRecordStore
{

	public SQLiteRecordStore(StorageClient client)
	{
		super(client);
		// TODO Auto-generated constructor stub
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
	protected ValueStringGenerator getValueStringGenerator()
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected boolean doesTableExist(String tableName)
	{
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	protected void executeQuery(String sql) throws Exception
	{
		// TODO Auto-generated method stub
		
	}
	
	@Override
	protected SchemaInfoGenerator getSchemaInfoGenerator()
	{
		return new SchemaInfoGenerator()
		{
			
			@Override
			public void visit(ByteArrayColumn byteArrayCol)
			{
				schemaInfo.addColumn(new ColumnPointer(byteArrayCol), byteArrayCol.getName(), "BLOB");
			}
			
			@Override
			public void visit(OrientationColumn orCol)
			{
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visit(LocationColumn locCol)
			{
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visit(ForeignKeyColumn foreignKeyCol)
			{
				// TODO Auto-generated method stub
				
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
			public void visit(StringColumn stringCol)
			{
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visit(IntegerColumn intCol)
			{
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visit(FloatColumn floatCol)
			{
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visit(DateTimeColumn dateTimeCol)
			{
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void visit(BooleanColumn boolCol)
			{
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void enter(RecordColumn<?> recordCol)
			{
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void leave(RecordColumn<?> recordCol)
			{
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public boolean allowOrientationSelfTraversal()
			{
				// TODO Auto-generated method stub
				return false;
			}
			
			@Override
			public boolean allowLocationSelfTraversal()
			{
				// TODO Auto-generated method stub
				return false;
			}
			
			@Override
			public boolean allowForeignKeySelfTraversal()
			{
				// TODO Auto-generated method stub
				return false;
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
				// do nothing
			}
			
		};
	}

}
