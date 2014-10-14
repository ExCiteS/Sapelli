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

package uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.java;

import java.io.File;
import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.io.FileHelpers;
import uk.ac.ucl.excites.sapelli.shared.util.TimeUtils;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.ISQLiteCursor;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore;
import uk.ac.ucl.excites.sapelli.storage.model.Model;

import com.almworks.sqlite4java.SQLiteBackup;
import com.almworks.sqlite4java.SQLiteConnection;
import com.almworks.sqlite4java.SQLiteException;

/**
 * A RecordStore class which stores records in a SQLite database, using the sqlite4java library.
 * 
 * @author mstevens
 */
public class JavaSQLiteRecordStore extends SQLiteRecordStore
{

	private SQLiteConnection db;
	
	/**
	 * @param client
	 * @param context
	 * @param dbName
	 * @throws Exception 
	 */
	public JavaSQLiteRecordStore(StorageClient client, File folderPath, String baseName) throws Exception
	{
		super(client);
		
		// Open database connection:
		this.db = new SQLiteConnection(new File(folderPath, baseName + DATABASE_NAME_SUFFIX + "." + DATABASE_FILE_EXTENSION));
		db.open(true);
		
		// Initialise:
		initialise(!doesTableExist(client.getTableName(Model.MODEL_SCHEMA)));
	}
	
	@Override
	protected void executeSQL(String sql) throws DBException
	{
		System.out.println("SQLite> Raw execute: " + sql);
		try
		{
			db.exec(sql);
		}
		catch(SQLiteException sqlE)
		{
			throw new DBException(sqlE);
		}
	}

	@Override
	protected ISQLiteCursor executeQuery(String sql, List<SQLiteColumn<?, ?>> paramCols, List<Object> sapArguments) throws DBException
	{
		// Get statement:
		JavaSQLiteStatement selectStatement = getStatement(sql, paramCols);
		
		// Bind parameters:
		selectStatement.bindAll(sapArguments);
		
		// Execute and return cursor:
		return selectStatement.executeSelectRows();
	}
	

	@Override
	protected JavaSQLiteStatement getStatement(String sql, List<SQLiteColumn<?, ?>> paramCols) throws DBException
	{
		try
		{
			System.out.println("SQLite> Compile/reuse statement: " + sql); // TODO remove debug logging
			return new JavaSQLiteStatement(db, sql, paramCols);
		}
		catch(SQLiteException sqliteE)
		{
			throw new DBException("Exception upon compiling SQL: " + sql, sqliteE);
		}
	}
	
	@Override
	protected void close() throws DBException
	{
		db.dispose();
	}
	
	/**
	 * Performs a back-up of the database using the SQLite Online Backup API
	 * 
	 * @see com.almworks.sqlite4java.SQLiteBackup
	 * @see <a href="http://www.sqlite.org/c3ref/backup_finish.html#sqlite3backupinit"> SQLite Online Backup API</a>
	 * @see <a href="http://www.sqlite.org/backup.html">Using the SQLite Online Backup API</a>
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore#doBackup(java.io.File)
	 */
	@Override
	protected void doBackup(File destinationFolder) throws DBException
	{
		File dstFile = new File(destinationFolder, FileHelpers.trimFileExtensionAndDot(db.getDatabaseFile().getName()) + BACKUP_SUFFIX + "_" + TimeUtils.getTimestampForFileName() + "." + DATABASE_FILE_EXTENSION);
		SQLiteBackup backup = null;
		try
		{
			backup = db.initializeBackup(dstFile);
			while(!backup.isFinished())
				backup.backupStep(32);
		}
		catch(SQLiteException e)
		{
			throw new DBException("Failed to back-up SQLite database to: " + dstFile.getAbsolutePath(), e);
		}
		finally
		{
			if(backup != null)
				backup.dispose();
		}
	}
	
	@Override
	protected File getDatabaseFile()
	{
		return db.getDatabaseFile();
	}

}
