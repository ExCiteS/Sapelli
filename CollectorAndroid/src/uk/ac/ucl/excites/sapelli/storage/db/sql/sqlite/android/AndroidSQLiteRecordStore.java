/**
 * Sapelli data collection platform: http://sapelli.org
 * 
 * Copyright 2012-2016 University College London - ExCiteS group
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
import java.util.List;

import android.annotation.TargetApi;
import android.content.Context;
import android.content.ContextWrapper;
import android.database.Cursor;
import android.database.DatabaseErrorHandler;
import android.database.SQLException;
import android.database.sqlite.SQLiteCursorDriver;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteDatabase.CursorFactory;
import android.database.sqlite.SQLiteException;
import android.database.sqlite.SQLiteOpenHelper;
import android.database.sqlite.SQLiteQuery;
import android.os.Build;
import android.util.Log;
import uk.ac.ucl.excites.sapelli.shared.db.exceptions.DBException;
import uk.ac.ucl.excites.sapelli.shared.util.ExceptionHelpers;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStoreUpgrader;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteCursor;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore;

/**
 * A RecordStore class which uses Android's SQLite facilities to store records.
 * 
 * @author mstevens
 */
public class AndroidSQLiteRecordStore extends SQLiteRecordStore
{

	// STATIC----------------------------------------------
	static private final String TAG = "SQLite";
	
	// DYNAMIC---------------------------------------------
	private final SQLiteDatabase db;
	
	private AndroidSQLiteStatement selectChangesStatement;
	
	/**
	 * @param client
	 * @param context
	 * @param databaseFolder
	 * @param baseName
	 * @param targetVersion
	 * @param upgrader
	 * @throws DBException
	 */
	public AndroidSQLiteRecordStore(StorageClient client, Context context, File databaseFolder, String baseName, int targetVersion, SQLRecordStoreUpgrader upgrader) throws DBException
	{
		super(client);
		
		// Helper:
		CustomSQLiteOpenHelper helper = new CustomSQLiteOpenHelper(new CollectorContext(context, databaseFolder), GetDBFileName(baseName), new AndroidSQLiteCursorFactory(), targetVersion);
		
		// Open writable database:
		try
		{
			this.db = helper.getWritableDatabase();
		}
		catch(SQLiteException sqliteE)
		{
			throw new DBException("Failed to open writable SQLite database", sqliteE);
		}
		if(loggingEnabled)
			Log.d(TAG, "Opened SQLite database: " + db.getPath());
		
		// Set initialisation args:
		setInitialisationArguments(helper.newDB, targetVersion, upgrader);
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore#doInitialise()
	 */
	@Override
	protected void doInitialise() throws DBException
	{
		addProtectedTable("android_metadata");
		
		super.doInitialise(); // !!!
	}
	
	@Override
	protected void executeSQL(String sql) throws DBException
	{
		if(loggingEnabled)
			Log.d(TAG, "Raw execute: " + sql);
		try
		{
			db.execSQL(sql);
		}
		catch(SQLException sqlE)
		{
			throw new DBException("Exception upon executing SQL: " + sql, sqlE);
		}
	}

	@Override
	protected int executeSQLReturnAffectedRows(String sql) throws DBException
	{
		executeSQL(sql);
		return getNumberOfAffectedRows();
	}
	
	@Override
	public boolean _isInTransaction()
	{
		return db.inTransaction();
	}

	/**
	 * In SQlite basic transactions (those controlled with BEGIN...COMMIT/ROLLBACK) cannot
	 * be nested (for that one needs to use the SAVEPOINT and RELEASE commands, which we won't
	 * use here). However, for flexibility reasons we will pretend that it is possible (i.e. we
	 * don't throw an exception if a request arrives to open a 2nd, 3rd, etc. transaction).
	 * 
	 * The Android SQLite API seems to suggest it does support nested transactions but this is
	 * just an "illusion" because the inner transactions cannot be individually rolled-back.
	 * Given that {@link SQLiteRecordStore} already has its own (similar) nested-transaction
	 * simulation we don't use Android's nested transactions.
	 * 
	 * @see <a href="http://sqlite.org/lang_transaction.html">http://sqlite.org/lang_transaction.html</a>
	 * 
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore#doStartTransaction()
	 */
	@Override
	protected void doStartTransaction() throws DBException
	{
		if(!isInTransaction()) // only the first/outer transaction is real, all nested/inner transactions are simulated
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
	protected void doRollbackTransaction()
	{
		if(numberOfOpenTransactions() == 1) // higher numbers indicate nested transactions which are simulated
			try
			{
				db.endTransaction();
			}
			catch(SQLiteException sqliteE)
			{
				client.logError("Could not roll-back SQLite transaction: " + ExceptionHelpers.getMessageAndCause(sqliteE));
			}
	}

	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore#executeQuery(java.lang.String, java.util.List, java.util.List)
	 */
	@Override
	protected SQLiteCursor executeQuery(String sql, List<SQLiteColumn<?, ?>> paramCols, List<? extends Object> sapArguments) throws DBException
	{
		try
		{
			// Build selection arguments array:
			String[] argStrings = new String[paramCols.size()];
			for(int p = 0; p < argStrings.length; p++)
				argStrings[p] = paramCols.get(p).sapelliObjectToLiteral(sapArguments.get(p), false);
			
			// Log query & arguments:
			if(isLoggingEnabled())
				Log.d(TAG, getQueryLogMessage(sql, paramCols, sapArguments));
			
			// Execute:
			return (AndroidSQLiteCursor) db.rawQuery(sql, argStrings);
		}
		catch(SQLException e)
		{
			Log.d(TAG, "Error: Failed to execute raw SQLite query (" + sql + ").", e);
			throw new DBException("Failed to execute SQLite selection query: " + sql, e);
		}
	}
	
	@Override
	public int getVersion() throws DBException
	{
		try
		{
			return db.getVersion();
		}
		catch(Exception ex)
		{
			throw new DBException("Could not get database version.", ex);
		}
	}

	@Override
	protected void setVersion(int version) throws DBException
	{
		try
		{
			db.setVersion(version);
		}
		catch(Exception ex)
		{
			throw new DBException("Could not set database version.", ex);
		}
	}
	
	/* (non-Javadoc)
	 * @see uk.ac.ucl.excites.sapelli.storage.db.sql.SQLRecordStore#release()
	 */
	@Override
	protected void release()
	{
		if(selectChangesStatement != null)
		{
			selectChangesStatement.close();
			selectChangesStatement = null;
		}
		
		super.release(); // !!!
	}
	
	@Override
	protected void closeConnection()
	{
		db.close();
	}
	
	@Override
	protected File getDatabaseFile()
	{
		return new File(db.getPath());
	}

	@Override
	protected AndroidSQLiteStatement generateStatement(String sql, List<SQLiteColumn<?, ?>> paramCols) throws DBException
	{
		if(loggingEnabled)
			Log.d(TAG, "Compile statement: " + sql);
		try
		{
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
	public int getNumberOfAffectedRows() throws DBException//SQLException
	{
		if(selectChangesStatement == null)
			selectChangesStatement = generateStatement("SELECT changes();", null);
		return selectChangesStatement.executeLongQuery().intValue();
		// Alternative implementation (kept for future reference only):
		/*Cursor cursor = null;
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
			StreamHelpers.SilentClose(cursor);
		}*/
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
	 * Custom {@link SQLiteOpenHelper} implementation.
	 * 
	 * Important:
	 * 	After calling onUpgrade() (provided no Exception is thrown) the SQLiteOpenHelper will already
	 * 	set the new version (see {@link android.database.sqlite.SQLiteOpenHelper#getDatabaseLocked(boolean)})!
	 * 	Because we don't do the actual upgrade work in our onUpgrade() implementation we must undo this in
	 * 	{@link #onOpen(SQLiteDatabase)} which is called soon after.
	 * 
	 * @author mstevens
	 */
	static private class CustomSQLiteOpenHelper extends SQLiteOpenHelper
	{
		
		private boolean newDB = false;
		private int dbVersion;
		
		public CustomSQLiteOpenHelper(Context context, String name, CursorFactory factory, int targetVersion)
		{
			super(context, name, factory, targetVersion);
			dbVersion = targetVersion;
		}
		
		@Override
		public void onCreate(SQLiteDatabase db)
		{
			newDB = true;
		}
		
		/* (non-Javadoc)
		 * @see android.database.sqlite.SQLiteOpenHelper#onUpgrade(android.database.sqlite.SQLiteDatabase, int, int)
		 */
		@Override
		public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion)
		{
			this.dbVersion = oldVersion; // !!!
		}

		/* (non-Javadoc)
		 * @see android.database.sqlite.SQLiteOpenHelper#onOpen(android.database.sqlite.SQLiteDatabase)
		 */
		@Override
		public void onOpen(SQLiteDatabase db)
		{
			db.setVersion(dbVersion); // !!!
		}
		
	}
	
	/**
	 * Custom cursor factory, this enables us to our custom cursor class ({@link AndroidSQLiteCursor}) when processing query results.
	 * Another helpful aspect is the ability to log queries for debugging.
	 * 
	 * @author mstevens
	 */
	private final class AndroidSQLiteCursorFactory implements CursorFactory
	{
		
		/**
		 * Uncomment the Log line in this method if there is a suspicion Android's classes may be modifying the SQL string,
		 * meaning the executed query is different from what is being logged in {@link #executeQuery(String, List, List)}.
		 * 
		 * @see android.database.sqlite.SQLiteDatabase.CursorFactory#newCursor(android.database.sqlite.SQLiteDatabase, android.database.sqlite.SQLiteCursorDriver, java.lang.String, android.database.sqlite.SQLiteQuery)
		 * @see SQLiteQuery#toString()
		 */
		@Override
		public Cursor newCursor(SQLiteDatabase db, SQLiteCursorDriver masterQuery, String editTable, SQLiteQuery query)
		{
			// This is already logged in executeQuery():
			/*if(loggingEnabled)
				Log.d(TAG, "Executing query: " + query.toString().substring("SQLiteQuery: ".length()));*/			
			return AndroidSQLiteCursor.newCursor(db, masterQuery, editTable, query);
		}
	}
	
	/**
	 * Our custom cursor class, which behaves identical to the {@link SQLiteCursor} super class. The only difference
	 * is it implements the {@link SQLiteCursor} interface. Apart from {@link #hasRow()} all methods declared in
	 * the interface already exist in the {@link SQLiteCursor}. The purpose of this strategy is to allow non-Android
	 * specific classes (i.e. at the level of the Sapelli Library), notably the typed SQLiteColumn subclasses, to
	 * call methods on cursor instances.
	 * 
	 * @author mstevens
	 */
	static private final class AndroidSQLiteCursor extends android.database.sqlite.SQLiteCursor implements SQLiteCursor
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
