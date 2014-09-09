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
import uk.ac.ucl.excites.sapelli.storage.db.sql.SQLTable;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.ISQLiteCursor;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteRecordStore;
import uk.ac.ucl.excites.sapelli.storage.db.sql.sqlite.SQLiteStatement;
import uk.ac.ucl.excites.sapelli.storage.model.Record;
import uk.ac.ucl.excites.sapelli.storage.queries.RecordsQuery;
import android.annotation.TargetApi;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteCursor;
import android.database.sqlite.SQLiteCursorDriver;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteQuery;
import android.database.sqlite.SQLiteDatabase.CursorFactory;
import android.database.sqlite.SQLiteOpenHelper;
import android.os.Build;
import android.util.Log;

/**
 * @author mstevens
 *
 */
public class AndroidSQLiteRecordStore extends SQLiteRecordStore
{
	
	static public final int DATABASE_VERSION = 2;
	
	private SQLiteDatabase db;
	private boolean newDBFile = false;

	/**
	 * @param client
	 * @param context
	 * @param dbName
	 * @throws Exception 
	 */
	public AndroidSQLiteRecordStore(StorageClient client, Context context, String dbName) throws Exception
	{
		super(client);
		
		CursorFactory cursorFactory =  null;
//			new CursorFactory()
//		{
//			
//			@Override
//			public Cursor newCursor(SQLiteDatabase db, SQLiteCursorDriver masterQuery, String editTable, SQLiteQuery query)
//			{
//				return new AndroidSQLiteCursor(db, editTable,
//			}
//		};
		
		this.db = new SQLiteOpenHelper(context, dbName, cursorFactory, DATABASE_VERSION)
		{
			
			@Override
			public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion)
			{
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void onCreate(SQLiteDatabase db)
			{
				newDBFile = true;
			}
		}.getWritableDatabase();
		
		initialise(newDBFile);
	}
	
	@Override
	protected void executeSQL(String sql) throws DBException
	{
		Log.d("SQLite Exec", sql); // TODO remove debug logging
		db.execSQL(sql);
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
	
	@Override
	protected void doBackup(File destinationFolder) throws DBException
	{
		// TODO Auto-generated method stub
		
	}
	
	@Override
	protected void doFinalise() throws DBException
	{
		db.close();
	}

	@Override
	protected boolean supportsConcurrentConnections()
	{
		return false;
	}

	@Override
	protected SQLiteStatement createStatement(String sql)
	{
		return new AndroidSQLiteStatement(db, sql);
	}

	@Override
	protected char getParamPlaceholder()
	{
		return AndroidSQLiteStatement.PARAM_PLACEHOLDER;
	}

	@Override
	protected void queryForRecords(SQLTable table, RecordsQuery query, List<Record> result)
	{
		WhereClauseGenerator selector = new WhereClauseGenerator(table, query.getConstraints(), false);
		Cursor cursor = db.query(table.name, null, selector.getClause(), /* TODO selector.getValues() */ null, null, table.getDatabaseColumn(query.getOrderBy()).name, query.isLimited() ? "LIMIT " + query.getLimit() : null);
		
		while (cursor.moveToNext())
		{
			// TODO
		}
	}
	
	private class AndroidSQLiteCursor extends SQLiteCursor implements ISQLiteCursor
	{

		@TargetApi(Build.VERSION_CODES.HONEYCOMB)
		public AndroidSQLiteCursor(SQLiteCursorDriver driver, String editTable, SQLiteQuery query)
		{
			super(driver, editTable, query);
		}
		
		public AndroidSQLiteCursor (SQLiteDatabase db, SQLiteCursorDriver driver, String editTable, SQLiteQuery query) 
		{
			super(db, driver, editTable, query);
		}
		
	}

}
