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

package uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.android;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.ISQLiteCursor;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore;
import android.annotation.TargetApi;
import android.content.Context;
import android.content.ContextWrapper;
import android.database.Cursor;
import android.database.DatabaseErrorHandler;
import android.database.SQLException;
import android.database.sqlite.SQLiteCursor;
import android.database.sqlite.SQLiteCursorDriver;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteDatabase.CursorFactory;
import android.database.sqlite.SQLiteException;
import android.database.sqlite.SQLiteOpenHelper;
import android.database.sqlite.SQLiteQuery;
import android.os.Build;
import android.util.Log;

/**
 * A RecordStore class which uses Android's SQLite facilities to store records.
 * 
 * @author mstevens
 */
public class AndroidSQLiteRecordStore extends SQLiteRecordStore
{

	// Statics----------------------------------------------	
	
	// Note there never was a database version 1 (because SQLite storage was introduced in Sapelli v2.0) 
	
	/**
	 * Used from "Sapelli Collector for Android" v2.0-beta-3 up to v2.0-beta-11
	 */
	static private final int DATABASE_VERSION_2 = 2;
	
	/**
	 * Used from "Sapelli Collector for Android" v2.0-beta-12 and newer
	 * 
	 * Reason for version increase:
	 * 	the introduction of the lastStoredAt, lastExportedAt & lastTransmittedAt columns
	 * 
	 * Because we are still in the beta phase _no_ "real" v2->v3 upgrade (i.e. adding those columns to
	 * existing tables and schemata objects) is carried out, instead the database file is backed-up and
	 * all existing tables and indexes are simply dropped (see {@link CustomSQLiteOpenHelper#onUpgrade(SQLiteDatabase, int, int)}).
	 */
	static private final int DATABASE_VERSION_3 = 3;
	
	/**
	 * The current database version number
	 */
	static public final int CURRENT_DATABASE_VERSION = DATABASE_VERSION_3;
	
	/**
	 * Helper method which returns a list with the names of all tables in the given database.
	 * 
	 * @param db
	 * @param includeAndroidMetadataTable
	 * @param includeSequenceTable
	 * @return
	 * @throws SQLiteException
	 */
	static private List<String> GetAllTables(SQLiteDatabase db, boolean includeAndroidMetadataTable, boolean includeSequenceTable) throws SQLiteException
	{
		List<String> tables = new ArrayList<String>();
		Cursor cursor = db.rawQuery("SELECT name FROM sqlite_master WHERE type='table';", null);
		cursor.moveToFirst();
		while(!cursor.isAfterLast())
		{
			String tableName = cursor.getString(0);
			if(	(includeAndroidMetadataTable || !tableName.equals("android_metadata")) &&
				(includeSequenceTable || !tableName.equals("sqlite_sequence")))
				tables.add(tableName);
			cursor.moveToNext();
		}
		cursor.close();
		return tables;
	}
	
	/**
	 * Helper method which returns a list with the names of all indexes in the given database.
	 * 
	 * @param db
	 * @return
	 * @throws SQLiteException
	 */
	static private List<String> GetAllIndexes(SQLiteDatabase db) throws SQLiteException
	{
		List<String> indexes = new ArrayList<String>();
		Cursor cursor = db.rawQuery("SELECT name FROM sqlite_master WHERE type='index';", null);
		cursor.moveToFirst();
		while(!cursor.isAfterLast())
		{			
			indexes.add(cursor.getString(0));
			cursor.moveToNext();
		}
		cursor.close();
		return indexes;
	}
	
	/**
	 * Helper method which drops all tables (except android_metadata) and indexes in the given database.
	 * 
	 * @param db
	 * @throws SQLiteException
	 */
	static private void DropAllTablesAndIndexes(SQLiteDatabase db) throws SQLiteException
	{
		// Tables:
		for(String tableName: GetAllTables(db, false, true))
			db.execSQL("DROP TABLE IF EXISTS " + tableName);
		// Indexes:
		for(String indexName: GetAllIndexes(db))
			db.execSQL("DROP INDEX IF EXISTS " + indexName);
	}

	// Dynamics---------------------------------------------
	private SQLiteDatabase db;
	
	/**
	 * @param client
	 * @param context
	 * @param databaseFolder
	 * @param baseName
	 * @throws DBException
	 */
	public AndroidSQLiteRecordStore(StorageClient client, Context context, File databaseFolder, String baseName) throws DBException
	{
		super(client);
		
		// Helper:
		CustomSQLiteOpenHelper helper = new CustomSQLiteOpenHelper(context, databaseFolder, baseName);		
		
		// Open writable database:
		try
		{
			this.db = helper.getWritableDatabase();
		}
		catch(SQLiteException sqliteE)
		{
			throw new DBException("Failed to open writable SQLite database", sqliteE);
		}
		Log.d("SQLite", "Opened SQLite database: " + db.getPath()); // TODO remove debug logging
		
		// Initialise:
		initialise(helper.newDB);
	}
	
	@Override
	protected void executeSQL(String sql) throws DBException
	{
		Log.d("SQLite", "Raw execute: " + sql); // TODO remove debug logging
		try
		{
			db.execSQL(sql);
		}
		catch(SQLException sqlE)
		{
			throw new DBException(sqlE);
		}
	}

	@Override
	protected int executeSQLReturnAffectedRows(String sql) throws DBException
	{
		executeSQL(sql);
		return getNumberOfAffectedRows();
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
				db.beginTransaction();
			}
			catch(Exception ex)
			{
				throw new DBException("Could not open SQLite transaction", ex);
			}
	}

	@Override
	protected void doCommitTransaction() throws DBException
	{
		if(numberOfOpenTransactions() == 1) // higher numbers indicate nested transactions which are simulated
			try
			{
				db.setTransactionSuccessful();
				db.endTransaction();
			}
			catch(Exception ex)
			{
				throw new DBException("Could not commit SQLite transaction", ex);
			}
	}

	@Override
	protected void doRollbackTransaction() throws DBException
	{
		if(numberOfOpenTransactions() == 1) // higher numbers indicate nested transactions which are simulated
			try
			{
				db.endTransaction();
			}
			catch(Exception ex)
			{
				throw new DBException("Could not roll-back SQLite transaction", ex);
			}
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore#executeQuery(java.lang.String, java.util.List, java.util.List)
	 */
	@Override
	protected ISQLiteCursor executeQuery(String sql, List<SQLiteColumn<?, ?>> paramCols, List<Object> sapArguments) throws DBException
	{
		try
		{
			// Build selectionArgs array:
			String[] argStrings = new String[paramCols.size()];
			for(int p = 0; p < argStrings.length; p++)
				argStrings[p] = paramCols.get(p).sapelliObjectToLiteral(sapArguments.get(p), false);
			// Execute
			return (AndroidSQLiteCursor) db.rawQuery(sql, argStrings);
		}
		catch(SQLException e)
		{
			Log.e("SQLite_Error", "Failed to execute raw query (" + sql + ").", e);
			throw new DBException("Failed to execute selection query: " + sql, e);
		}
	}
	
	@Override
	protected void close()
	{
		db.close();
	}
	
	@Override
	protected File getDatabaseFile()
	{
		return new File(db.getPath());
	}

	@Override
	protected AndroidSQLiteStatement getStatement(String sql, List<SQLiteColumn<?, ?>> paramCols) throws DBException
	{
		try
		{
			Log.d("SQLite", "Compile statement: " + sql); // TODO remove debug logging
			return new AndroidSQLiteStatement(this, db.compileStatement(sql), paramCols);
		}
		catch(SQLException sqlE)
		{
			throw new DBException("Exception upon compiling SQL: " + sql, sqlE);
		}
	}
	
	/**
	 * Returns the number of database rows that were changed or inserted or deleted by the most recently completed INSERT, DELETE, or UPDATE statement
	 * 
	 * @return the number of affected database rows
	 * @throws SQLiteException
	 * 
	 * @see http://www.sqlite.org/lang_corefunc.html#changes
	 * @see http://stackoverflow.com/a/6659693/1084488
	 * @see http://stackoverflow.com/a/18441056/1084488
	 */
	public int getNumberOfAffectedRows() throws SQLException
	{
		Cursor cursor = null;
		try
		{
		    cursor = db.rawQuery("SELECT changes();", null);
		    if(cursor != null && cursor.moveToFirst())
		        return (int) cursor.getLong(0);
		    else
		    	throw new SQLException("Failure on execution of changes() query");
		}
		finally
		{
		    if(cursor != null)
		        cursor.close();
		}
	}
	
	/**
	 * @author mstevens
	 *
	 */
	private class CustomSQLiteOpenHelper extends SQLiteOpenHelper
	{
		
		private boolean newDB = false;
		
		public CustomSQLiteOpenHelper(Context context, File databaseFolder, String baseName)
		{			
			super(new CollectorContext(context, databaseFolder), GetDBFileName(baseName), new AndroidSQLiteCursorFactory(), CURRENT_DATABASE_VERSION);
		}
		
		@Override
		public void onCreate(SQLiteDatabase db)
		{
			newDB = true;
		}
		
		@Override
		public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion)
		{
			Log.w("SQLite", "Upgrading from database version " + oldVersion + " to version " + newVersion);
			
			// TODO backup !
			
			switch(oldVersion)
			{
				case DATABASE_VERSION_2 :
					Log.w("SQLite", "Dropping all tables and indexes");
					DropAllTablesAndIndexes(db); // see javadoc for DATABASE_VERSION_3
					break;
				// add cases for future versions here
			}
		}
		
	}

	/**
	 * Custom ContextWrapper which creates databases in the given folder, instead of in the
	 * internal application data folder (which the default Context implementation would do).
	 * 
	 * This allows us to place the SQLite database(s) on an external storage location (with the rest of the Sapelli files and folders).
	 * 
	 * Note that class makes assumptions about how {@link SQLiteOpenHelper} uses the provided {@link Context} to determine the database path,
	 * refer to the links below for details.
	 * 
	 * @author mstevens
	 * 
	 * @see http://stackoverflow.com/a/9168969/1084488
	 * @see http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/2.3.3_r1/android/database/sqlite/SQLiteOpenHelper.java#95
	 * @see http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/5.L_preview/android/database/sqlite/SQLiteOpenHelper.java#192
	 * @see http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/2.3.3_r1/android/app/ContextImpl.java#542
	 * @see http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/2.3.3_r1/android/app/ContextImpl.java#560
	 * @see http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/5.L_preview/android/app/ContextImpl.java#940
	 * @see http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/5.L_preview/android/app/ContextImpl.java#968
	 */
	static private class CollectorContext extends ContextWrapper
	{

		private final File databaseFolder;

		public CollectorContext(Context baseContext, File databaseFolder)
		{
			super(baseContext);
			this.databaseFolder = databaseFolder;
		}

		@Override
		public File getDatabasePath(String name)
		{
			return new File(databaseFolder, name);
		}

		@Override
		public SQLiteDatabase openOrCreateDatabase(String name, int mode, CursorFactory factory)
		{
			return SQLiteDatabase.openOrCreateDatabase(getDatabasePath(name), factory);
		}

		@TargetApi(Build.VERSION_CODES.HONEYCOMB)
		@Override
		public SQLiteDatabase openOrCreateDatabase(String name, int mode, CursorFactory factory, DatabaseErrorHandler errorHandler)
		{
			return SQLiteDatabase.openOrCreateDatabase(getDatabasePath(name).getAbsolutePath(), factory, errorHandler);
		}
		
	}
	
	/**
	 * Custom cursor factory, this enables us to our custom cursor class ({@link AndroidSQLiteCursor}) when processing query results.
	 * Another helpful aspect is the ability to log queries for debugging.
	 * 
	 * @author mstevens
	 */
	static private class AndroidSQLiteCursorFactory implements CursorFactory
	{
	
		@Override
		public Cursor newCursor(SQLiteDatabase db, SQLiteCursorDriver masterQuery, String editTable, SQLiteQuery query)
		{
			Log.d("SQLite", query.toString()); // TODO remove debug logging
			return AndroidSQLiteCursor.newCursor(db, masterQuery, editTable, query);
		}
	}
	
	/**
	 * Our custom cursor class, which behaves identical to the {@link SQLiteCursor} super class. The only difference
	 * is it implements the {@link ISQLiteCursor} interface. Apart from {@link #hasRow()} all methods declared in
	 * the interface already exist in the {@link SQLiteCursor}. The purpose of this strategy is to allow non-Android
	 * specific classes (i.e. at the level of the Sapelli Library), notably the typed SQLiteColumn subclasses, to
	 * call methods on cursor instances.
	 * 
	 * @author mstevens
	 */
	static private class AndroidSQLiteCursor extends SQLiteCursor implements ISQLiteCursor
	{

		public static AndroidSQLiteCursor newCursor(SQLiteDatabase db, SQLiteCursorDriver driver, String editTable, SQLiteQuery query)
		{
			if(android.os.Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB)
				return new AndroidSQLiteCursor(driver, editTable, query);
			else
				return new AndroidSQLiteCursor(db, driver, editTable, query);
		}
		
		@TargetApi(Build.VERSION_CODES.HONEYCOMB)
		private AndroidSQLiteCursor(SQLiteCursorDriver driver, String editTable, SQLiteQuery query)
		{
			super(driver, editTable, query);
		}
		
		@SuppressWarnings("deprecation")
		private AndroidSQLiteCursor(SQLiteDatabase db, SQLiteCursorDriver driver, String editTable, SQLiteQuery query) 
		{
			super(db, driver, editTable, query);
		}

		@Override
		public boolean hasRow()
		{
			return getCount() > 0;
		}
		
	}

}
