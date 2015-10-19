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
import uk.ac.ucl.excites.sapelli.shared.io.StreamHelpers;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStoreUpgrader;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteCursor;
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
	 * @param folderPath
	 * @param baseName
	 * @param targetVersion
	 * @param upgrader
	 * @throws DBException
	 */
	public JavaSQLiteRecordStore(StorageClient client, File folderPath, String baseName, int targetVersion, SQLRecordStoreUpgrader upgrader) throws DBException
	{
		super(client);
		
		// Database file:
		File dbFile = new File(folderPath, GetDBFileName(baseName));
		boolean newDB = !dbFile.exists();
		
		// Open database connection:
		try
		{
			this.db = new SQLiteConnection(dbFile);
			db.open(true); // allow creation
		}
		catch(SQLiteException sqlE)
		{
			throw new DBException(sqlE);
		}
		
		// Just to be sure:
		newDB = newDB || !doesTableExist(Model.MODEL_SCHEMA);
		
		// Set init args:
		setInitialisationArguments(newDB, targetVersion, upgrader);
	}
	
	@Override
	public int getVersion() throws DBException
	{
		JavaSQLiteStatement statement = null;
		try
		{
			statement = getStatement("PRAGMA user_version;", null);
			return statement.executeLongQuery().intValue();
		}
		catch(Exception ex)
		{
			throw new DBException("Could not get database version.", ex.getCause());
		}
		finally
		{
			StreamHelpers.SilentClose(statement);
		}
	}

	@Override
	protected void setVersion(int version) throws DBException
	{
		try
		{
			executeSQL("PRAGMA user_version = " + version + ";");
		}
		catch(Exception ex)
		{
			throw new DBException("Could not set database version.", ex.getCause());
		}
	}
	
	@Override
	protected void executeSQL(String sql) throws DBException
	{
		if(loggingEnabled)
			System.out.println("SQLite> Raw execute: " + sql);
		try
		{
			db.exec(sql);
		}
		catch(SQLiteException sqlE)
		{
			throw new DBException("Exception upon executing SQL: " + sql, sqlE);
		}
	}
	
	@Override
	protected int executeSQLReturnAffectedRows(String sql) throws DBException
	{
		// Execute SQL:
		executeSQL(sql);
		// Return number of affected rows:
		try
		{
			int rows = db.getChanges();
			if(loggingEnabled)
				System.out.println("SQLite> Affected rows: " + rows);
			return rows;
		}
		catch(SQLiteException e)
		{
			throw new DBException("Failed to get number of changed rows", e);
		}
	}

	@Override
	protected SQLiteCursor executeQuery(String sql, List<SQLiteColumn<?, ?>> paramCols, List<? extends Object> sapArguments) throws DBException
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
			if(loggingEnabled)
				System.out.println("SQLite> Compile/reuse statement: " + sql);
			return new JavaSQLiteStatement(db, sql, paramCols);
		}
		catch(SQLiteException sqliteE)
		{
			throw new DBException("Exception upon compiling SQL: " + sql, sqliteE);
		}
	}
	
	@Override
	protected void closeConnection() throws DBException
	{
		db.dispose();
	}
	
	/**
	 * Performs a back-up of the database using the SQLite Online Backup API
	 * 
	 * @see com.almworks.sqlite4java.SQLiteBackup
	 * @see <a href="http://www.sqlite.org/c3ref/backup_finish.html#sqlite3backupinit">SQLite Online Backup API</a>
	 * @see <a href="http://www.sqlite.org/backup.html">Using the SQLite Online Backup API</a>
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore#doBackup(java.io.File)
	 */
	@Override
	protected void doBackup(File destinationFile) throws Exception
	{
		SQLiteBackup backup = null;
		try
		{
			backup = db.initializeBackup(destinationFile);
			while(!backup.isFinished())
				backup.backupStep(32);
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
