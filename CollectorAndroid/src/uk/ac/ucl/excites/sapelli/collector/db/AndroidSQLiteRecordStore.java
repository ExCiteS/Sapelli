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

import uk.ac.ucl.excites.sapelli.shared.db.DBException;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore;

/**
 * @author mstevens
 *
 */
public class AndroidSQLiteRecordStore extends SQLiteRecordStore
{
	
	public AndroidSQLiteRecordStore(StorageClient client, File folder, String baseFilename) throws Exception
	{
		super(client, folder, baseFilename);
	}
	
	@Override
	protected void executeQuery(String sql) throws DBException
	{
		System.out.println("Execute SQL: " + sql);
	}
	
	@Override
	protected void doBackup(File destinationFolder) throws DBException
	{
		// TODO Auto-generated method stub
		
	}
	
	@Override
	protected void doFinalise() throws DBException
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	protected boolean supportsConcurrentConnections()
	{
		return false;
	}

}
