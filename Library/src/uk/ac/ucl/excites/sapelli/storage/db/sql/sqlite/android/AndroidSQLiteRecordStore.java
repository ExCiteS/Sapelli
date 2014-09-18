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
import java.util.List;

import uk.ac.ucl.excites.sapelli.shared.db.DBException;
import uk.ac.ucl.excites.sapelli.storage.StorageClient;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.ISQLiteCursor;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore;
import android.annotation.TargetApi;
import android.content.Context;
import android.database.Cursor;
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
	static public final int DATABASE_VERSION = 2;

	// Dynamics---------------------------------------------
	private SQLiteDatabase db;
	private boolean newDB = false;
	
	/**
	 * @param client
	 * @param context
	 * @param dbName
	 * @throws Exception 
	 */
	public AndroidSQLiteRecordStore(StorageClient client, Context context, String baseName) throws Exception
	{
		super(client);
		
		// Helper:
		SQLiteOpenHelper helper = new SQLiteOpenHelper(context, baseName + DATABASE_NAME_SUFFIX, new AndroidSQLiteCursorFactory(), DATABASE_VERSION)
		{
			@Override
			public void onCreate(SQLiteDatabase db)
			{
				newDB = true;
			}
			
			@Override
			public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion)
			{
				// TODO what to do here??
			}
		};
		
		// Open writable database:
		this.db = helper.getWritableDatabase();
		
		// Initialise:
		initialise(newDB);
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
		if(getOpenTransactions() == 1) // higher numbers indicate nested transactions which are simulated
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
		if(getOpenTransactions() == 1) // higher numbers indicate nested transactions which are simulated
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
				argStrings[p] = paramCols.get(p).sapelliObjectToLiteral(sapArguments.get(p), false); // TODO must the literals be quoted?
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
	protected void doFinalise() throws DBException
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
